module Utils where

tupleSum :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
tupleSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)
