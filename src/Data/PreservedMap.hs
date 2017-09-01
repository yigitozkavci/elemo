-- | This module is a preserved implementation of Data.IntMap. It means
-- that values cannot be deleted, and only way to insert element is to
-- append one. All indexing logic is abstracted away for the user.

{-# LANGUAGE TupleSections #-}

module Data.PreservedMap where

--------------------------------------------------------------------------------
import           Control.Arrow (first, second)
import           Control.Applicative (liftA2, pure)
import qualified Data.IntMap   as IM
import           Data.Maybe    (catMaybes, fromJust, fromMaybe, isJust)
import           Prelude       hiding (lookup)
--------------------------------------------------------------------------------

type Map v = IM.IntMap (Maybe v)

newtype PMRef a = PMRef Int
  deriving (Show, Eq)

empty :: Map v
empty = IM.empty

(<|) :: Maybe v -> Map v -> (PMRef v, Map v)
v <| m = (PMRef r, IM.insert r v m)
  where
    r = IM.size m

(|>) :: Map v -> Maybe v -> (PMRef v, Map v)
m |> v = v <| m

-- Appends that ignore refs
(<<|) :: Maybe v -> Map v -> Map v
v <<| m = snd (v <| m)

(|>>) :: Map v -> Maybe v -> Map v
m |>> v = snd (m |> v)

lookup :: PMRef v -> Map v -> Maybe v
lookup (PMRef k) m =
  fromMaybe Nothing $ IM.lookup k m -- Whether it doesn't exist or deleted, we return nothing.

(!!) :: Map v -> PMRef v -> Maybe v
m !! r = lookup r m

delete :: PMRef v -> Map v -> Map v
delete (PMRef k) = IM.update (const (Just Nothing)) k

update :: (Maybe v -> Maybe v) -> PMRef v -> IM.IntMap (Maybe v) -> IM.IntMap (Maybe v)
update f (PMRef k) = IM.update (Just . f) k

fromList :: [Maybe v] -> Map v
fromList = foldl (\m v -> snd (m |> v)) empty

elems :: Map v -> [v]
elems = catMaybes . IM.elems

assocs :: Map v -> [(PMRef v, Maybe v)]
assocs = Prelude.map (first PMRef) . IM.assocs

map :: (Maybe v -> Maybe n) -> Map v -> Map n
map = IM.map

-- | Map a monadic function and gather the results.
mapM :: Monad m => (Maybe v -> m (Maybe n)) -> Map v -> m (Map n)
mapM f m' = (fromList . Prelude.map snd) <$> mapM' f (assocs m')

mapM' :: Monad m => (Maybe v -> m (Maybe n)) -> [(PMRef a, Maybe v)] -> m [(PMRef a, Maybe n)]
mapM' f [] = return []
mapM' f ((ref, val):xs) = liftA2 (:) ((ref,) <$> f val) (mapM' f xs)

mapMWithKey :: Monad m => (PMRef v -> Maybe v -> m (Maybe n)) -> Map v -> m (Map n)
mapMWithKey f m' = (fromList . Prelude.map snd) <$> mapMWithKey' f (assocs m')

mapMWithKey' :: Monad m => (PMRef v -> Maybe v -> m (Maybe n)) -> [(PMRef v, Maybe v)] -> m [(PMRef v, Maybe n)]
mapMWithKey' f [] = return []
mapMWithKey' f ((ref, val):xs) = liftA2 (:) ((ref,) <$> f ref val) (mapMWithKey' f xs)

assocsJust :: Map v -> [(PMRef v, v)]
assocsJust = Prelude.map (second fromJust) . filter (isJust . snd) . Prelude.map (first PMRef) . IM.assocs

