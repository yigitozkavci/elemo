-- | This module is a preserved implementation of Data.IntMap. It means
-- that values cannot be deleted, and only way to insert element is to
-- append one. All indexing logic is abstracted away for the user.

module Data.PreservedMap where

import           Prelude         hiding (lookup)
import qualified Data.IntMap     as IM
import           Data.Maybe      (fromMaybe)

type Map v = IM.IntMap (Maybe v)

newtype PMRef = PMRef Int

empty :: Map v
empty = IM.empty

(<|) :: v -> Map v -> (PMRef, Map v)
v <| m = (PMRef r, IM.insert r (Just v) m)
  where
    r = IM.size m

(|>) :: Map v -> v -> (PMRef, Map v)
m |> v = v <| m

lookup :: PMRef -> Map v -> Maybe v
lookup (PMRef k) m =
  fromMaybe Nothing $ IM.lookup k m -- Whether it doesn't exist or deleted, we return nothing.

(!!) :: Map v -> PMRef -> Maybe v
m !! r = lookup r m

delete :: Int -> Map v -> Map v
delete = IM.update (const (Just Nothing))

fromList :: [v] -> Map v
fromList = foldl (\m v -> snd (m |> v)) empty
