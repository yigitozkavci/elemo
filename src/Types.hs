module Types where

import qualified Data.Map       as Map
import           Graphics.Gloss

data Tower = Tower
  { _damage :: Int
  , _image :: Picture
  } deriving (Show)

data UIObject =
    Floor Bool Picture -- (Floor isPlacable picture)
  | UITower Tower
  deriving (Show)

type TileMap = Map.Map (Int, Int) UIObject
type GlobalTime = Int -- In miliseconds
type Position = (Int, Int)

-- (1, 0) : Right
-- (0, -1): Down
-- (-1, 0): Left
-- (0, 1) : Top
type Direction = (Int, Int)
type Speed = Int

tileSize :: Int
tileSize = 28

