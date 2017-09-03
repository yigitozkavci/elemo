module Data.Map.Extra (
  module Data.Map,
  module Data.Map.Extra
) where

--------------------------------------------------------------------------------
import           Data.Map
--------------------------------------------------------------------------------

assocM_ :: Monad m => ((k, v) -> m ()) -> Map k v -> m ()
assocM_ f m = mapM_ f (assocs m)
