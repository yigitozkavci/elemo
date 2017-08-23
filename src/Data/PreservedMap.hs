-- | This module is a preserved implementation of Data.IntMap. It means
-- that values cannot be deleted, and only way to insert element is to
-- append one. All indexing logic is abstracted away for the user.

module Data.PreservedMap where

import           Prelude         hiding (lookup)
import qualified Data.IntMap     as IM
import           Data.Maybe      (fromMaybe, fromJust, catMaybes, isJust)
import           Control.Arrow   (first, second)

type Map v = IM.IntMap (Maybe v)

newtype PMRef a = PMRef Int
  deriving Show

empty :: Map v
empty = IM.empty

(<|) :: v -> Map v -> (PMRef v, Map v)
v <| m = (PMRef r, IM.insert r (Just v) m)
  where
    r = IM.size m

(|>) :: Map v -> v -> (PMRef v, Map v)
m |> v = v <| m

-- Appends that ignore refs
(<<|) :: v -> Map v -> Map v
v <<| m = snd (v <| m)

(|>>) :: Map v -> v -> Map v
m |>> v = snd (m |> v)

lookup :: PMRef v -> Map v -> Maybe v
lookup (PMRef k) m =
  fromMaybe Nothing $ IM.lookup k m -- Whether it doesn't exist or deleted, we return nothing.

(!!) :: Map v -> PMRef v -> Maybe v
m !! r = lookup r m

delete :: Int -> Map v -> Map v
delete = IM.update (const (Just Nothing))

fromList :: [v] -> Map v
fromList = foldl (\m v -> snd (m |> v)) empty

elems :: Map v -> [v]
elems = catMaybes . IM.elems

assocs :: Map v -> [(PMRef v, v)]
assocs = Prelude.map (second fromJust) . Prelude.filter (isJust . snd) . Prelude.map (first PMRef) . IM.assocs

map :: (Maybe v -> Maybe n) -> Map v -> Map n
map = IM.map
