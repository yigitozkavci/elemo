{-# LANGUAGE ScopedTypeVariables #-}

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

-- | Here is the algorithm:
--
-- From (3, 2) to (10, 15) we don't just want this particule to move with (1, 1) vector. Instead, we want to move with probabilities (7/20, 13/20). This way we'll achieve a natural movement flow.
moveTowards :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveTowards rand (x, y) (tX, tY) = tupleSum (x, y) movVec
  where
    xD = tX - x
    yD = tY - y
    sum :: Float = fromIntegral $ abs xD + abs yD
    xP = fromIntegral xD / sum
    yP = fromIntegral yD / sum
    decide :: Float -> Int
    decide t = if abs t * 100 > fromIntegral (rand `mod` 100) then sig t else 0
    movVec = (decide xP, decide yP)

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = floor $ sqrt $ fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

(%|>>) :: ASetter s t (PM.Map v) (PM.Map v) -> Maybe v -> s -> t
a %|>> b = a %~ (PM.|>> b)

a %|>>= b = a %= (PM.|>> b)
