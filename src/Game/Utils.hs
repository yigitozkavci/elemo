{-# LANGUAGE ScopedTypeVariables #-}

module Game.Utils where

--------------------------------------------------------------------------------
import           Control.Lens      (ASetter, both, (%=), (%~), (&))
--------------------------------------------------------------------------------
import qualified Data.PreservedMap as PM
--------------------------------------------------------------------------------

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

(%|>>) :: ASetter s t (PM.Map v) (PM.Map v) -> Maybe v -> s -> t
a %|>> b = a %~ (PM.|>> b)

a %|>>= b = a %= (PM.|>> b)
