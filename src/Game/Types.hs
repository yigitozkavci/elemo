{-# LANGUAGE TemplateHaskell #-}

module Game.Types where

--------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.Map          as Map
import           Graphics.Gloss
--------------------------------------------------------------------------------
import           Data.Monoid       ((<>))
import qualified Data.PreservedMap as PM
--------------------------------------------------------------------------------

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


type TileMap = Map.Map (Int, Int) UIObject

type MovingVecIterator = (GlobalTime, TileMap, PM.Map MovingObject) -> Picture -> Speed -> (Position, Direction) -> Maybe (Position, Direction)

data UIObject =
    Floor Bool Picture -- (Floor isPlacable picture)
  | UITower Tower
  deriving (Show)

data TowerLockState =
    TowerLocked (PM.PMRef MovingObject)
  | TowerNonLocked
  deriving (Show)

data Tower = Tower
  { _damage    :: Int
  , _image     :: Picture
  , _lockState :: TowerLockState
  } deriving (Show)

data MovingObject = MovingObject
  { _moPicture   :: Picture
  , _speed       :: Speed
  , _currVec     :: (Position, Direction)
  , _vecIterator :: MovingVecIterator
  }

instance Show MovingObject where
  show (MovingObject _pic speed currVec _it) = "MovingObject { speed = " <> show speed <> ", currVec = " <> show currVec <> " }"

makeLenses ''MovingObject
