module Game.Utils where

--------------------------------------------------------------------------------
import           Control.Lens      (ASetter, both, (%=), (%~), (&))
--------------------------------------------------------------------------------
import qualified Data.PreservedMap as PM
--------------------------------------------------------------------------------

tupleSum :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
tupleSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

-- | Don't know if Haskell already has this, too lazy to look for
sig :: (Ord a, Num a) => a -> Int
sig x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

moveTowards :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTowards (x, y) (tX, tY) = tupleSum (x, y) movVec
  where
    movVec = (tX - x, tY - y) & both %~ sig

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = floor $ sqrt $ fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

(%|>>) :: ASetter s t (PM.Map v) (PM.Map v) -> Maybe v -> s -> t
a %|>> b = a %~ (PM.|>> b)

a %|>>= b = a %= (PM.|>> b)
