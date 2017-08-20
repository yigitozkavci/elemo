module Game.Utils where

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
