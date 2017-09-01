{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}

module Game.Position where

import           Data.Bijection
import           Control.Lens             hiding (inside)
import           Control.Arrow            (first, second, (>>>))

tileSize :: Int
tileSize = 28

-- Class of positions that are convertible. This class is implemented just
-- for ability to use one function to convert back-and-forth between two positions.
class PosConvertible a b | a -> b where
  convertPos :: a -> b

instance PosConvertible TilePosition AbsolutePosition where
  convertPos = biTo posBi

instance PosConvertible AbsolutePosition TilePosition where
  convertPos = biFrom posBi

newtype TilePosition = TilePosition { unwrapTilepos :: (Int, Int) }
  deriving (Show, Eq, Ord)

newtype AbsolutePosition = AbsolutePosition (Float, Float)
  deriving (Show, Eq, Ord)

posBi :: TilePosition :<->: AbsolutePosition
posBi =
  Bi (\(TilePosition pos) -> AbsolutePosition (scalePos pos))
     (\(AbsolutePosition pos) -> TilePosition (unscalePos pos))

--------------------------------------------------------------------------------

class Position a b | a -> b where
  (+.) :: a -> a -> a
  (-.) :: a -> a -> a
  insidePos :: ((b, b) -> (b, b)) -> a -> a

instance Position TilePosition Int where
  TilePosition (x1, y1) +. TilePosition (x2, y2) = TilePosition (x1 + x2, y1 + y2)
  TilePosition (x1, y1) -. TilePosition (x2, y2) = TilePosition (x1 - x2, y1 - y2)
  insidePos f (TilePosition pos) = TilePosition (f pos)

instance Position AbsolutePosition Float where
  AbsolutePosition (x1, y1) +. AbsolutePosition (x2, y2) = AbsolutePosition (x1 + x2, y1 + y2)
  AbsolutePosition (x1, y1) -. AbsolutePosition (x2, y2) = AbsolutePosition (x1 - x2, y1 - y2)
  insidePos f (AbsolutePosition pos) = AbsolutePosition (f pos)

--------------------------------------------------------------------------------

scalePos :: (Int, Int) -> (Float, Float)
scalePos = both *~ tileSize >>> both %~ fromIntegral

scaleWith :: Int -> (Int, Int) -> (Float, Float)
scaleWith magn = both *~ magn >>> both %~ fromIntegral

unscalePos :: (Float, Float) -> (Int, Int)
unscalePos = (both %~ floor) >>> (both %~ (`div` tileSize))

