-- | This module is a preserved implementation of Data.IntMap. It means
-- that values cannot be deleted, and only way to insert element is to
-- append one. All indexing logic is abstracted away for the user.

module Data.PreservedMap where

import           Prelude         hiding (lookup)
import qualified Data.IntMap     as IM
import           Data.Maybe      (fromMaybe)

type Map v = IM.IntMap (Maybe v)

empty :: Map v
empty = IM.empty

(<|) :: v -> Map v -> Map v
v <| m = IM.insert (IM.size m) (Just v) m

(|>) :: Map v -> v -> Map v
m |> v = v <| m

lookup :: Int -> Map v -> Maybe v
lookup k m =
  fromMaybe Nothing $ IM.lookup k m -- Whether it doesn't exist or deleted, we return nothing.

(!!) :: Map v -> Int -> Maybe v
m !! k = lookup k m

delete :: Int -> Map v -> Map v
delete = IM.update (const (Just Nothing))

fromList :: [v] -> Map v
fromList = foldl (|>) empty
