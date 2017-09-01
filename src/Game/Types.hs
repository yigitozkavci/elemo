{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE NoMonomorphismRestriction       #-}

module Game.Types where

--------------------------------------------------------------------------------
import           Control.Arrow            (first, second, (>>>))
import           Control.Lens             hiding (inside)
import           Control.Monad.Logger     (LoggingT, MonadLogger,
                                           runStdoutLoggingT)
import           Control.Monad.State
import           Control.Monad.State.Lazy
import qualified Data.Map                 as Map
import           Graphics.Gloss
import           System.Random
--------------------------------------------------------------------------------
import           Data.Bijection
import qualified Data.Heap                as Heap
import           Data.Monoid              (mconcat, (<>))
import qualified Data.PreservedMap        as PM
import qualified Data.Sequence            as Seq
import qualified Data.Sequence.Queue      as Seq
import qualified Data.Text                as T
import           Game.Assets
import           Game.Utils
import           Data.Fixed               (mod')
--------------------------------------------------------------------------------

data GUIState = GUIState
  { _guiTowers      :: Seq.Seq Tower
  , _guiTowerPosMap :: Map.Map (Int, Int) Tower
  }

initGUIState :: Assets -> GUIState
initGUIState assets = GUIState
  { _guiTowers = Seq.fromList
    [ Tower 10 (_whiteTower assets) 50 20 TowerNonLocked
    , Tower 20 (_greenTower assets) 100 40 TowerNonLocked
    ]
  , _guiTowerPosMap = Map.empty
  }

type SchedEvent = Heap.Entry GlobalTime (T.Text, SW ())
type SchedEventHeap = Heap.Heap SchedEvent

data SelectorState =
    MouseFree
  | SelectedItem Tower

data World = World
  { _level         :: Int
  , _levelPic      :: Picture
  , _monsters      :: PM.Map Monster
  , _wTileMap      :: TileMap
  , _globalTime    :: GlobalTime -- | In milisecs
  , _schedEvents   :: SchedEventHeap -- | Events can mutate the whole world, but
                                     -- we entrust them not to modify @_schedEvents@. That can mess things up.
  , _selectorState :: SelectorState
  , _mousePos      :: (Float, Float)
  , _guiState      :: GUIState
  , _assets        :: Assets
  , _builtTowers   :: TileMap
  , _randGen       :: StdGen
  , _projectiles   :: PM.Map Projectile
  , _playerInfo    :: PlayerInfo
  , _alerts        :: Seq.Queue T.Text
  }

newtype SW a = SW { runSW :: StateT World (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadState World, MonadLogger, MonadIO)

type GlobalTime = Int -- In miliseconds

-- (1, 0) : Right
-- (0, -1): Down
-- (-1, 0): Left
-- (0, 1) : Top
type Direction = (Int, Int)
type Speed = Int
type Damage = Int

tileSize :: Int
tileSize = 28

class HasPicture m where
  getPicture :: m -> Picture

--------------------------------------------------------------------------------

-- Class of positions that are convertible. This class is implemented just
-- for ability to use one function to convert back-and-forth between two positions.
class PosConvertible a b | a -> b where
  convertPos :: a -> b

posBi :: TilePosition :<->: AbsolutePosition
posBi =
  Bi (\(TilePosition pos) -> AbsolutePosition (scalePos pos))
     (\(AbsolutePosition pos) -> TilePosition (unscalePos pos))

newtype TilePosition = TilePosition { unwrapTilepos :: (Int, Int) }
  deriving (Show, Eq, Ord)

newtype AbsolutePosition = AbsolutePosition (Float, Float)
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

class Position a b | a -> b where
  (+.) :: a -> a -> a
  (-.) :: a -> a -> a
  inside :: ((b, b) -> (b, b)) -> a -> a

instance Position TilePosition Int where
  TilePosition (x1, y1) +. TilePosition (x2, y2) = TilePosition (x1 + x2, y1 + y2)
  TilePosition (x1, y1) -. TilePosition (x2, y2) = TilePosition (x1 - x2, y1 - y2)
  inside f (TilePosition pos) = TilePosition (f pos)

instance Position AbsolutePosition Float where
  AbsolutePosition (x1, y1) +. AbsolutePosition (x2, y2) = AbsolutePosition (x1 + x2, y1 + y2)
  AbsolutePosition (x1, y1) -. AbsolutePosition (x2, y2) = AbsolutePosition (x1 - x2, y1 - y2)
  inside f (AbsolutePosition pos) = AbsolutePosition (f pos)

--------------------------------------------------------------------------------

instance PosConvertible TilePosition AbsolutePosition where
  convertPos = biTo posBi

instance PosConvertible AbsolutePosition TilePosition where
  convertPos = biFrom posBi

--------------------------------------------------------------------------------

type TileMap = Map.Map TilePosition UIObject

type MonsterIterator = Speed -> (AbsolutePosition, TilePosition) -> SW (Maybe (AbsolutePosition, TilePosition))

type ProjectileIterator = (GlobalTime, PM.Map Monster) -> Speed -> Damage -> AbsolutePosition -> SW (Maybe AbsolutePosition)

data UIObject =
    Floor Bool Picture -- (Floor isPlacable picture)
  | UITower Tower
  deriving (Show)

instance HasPicture UIObject where
  getPicture (Floor _ pic)                    = pic
  getPicture (UITower Tower { _image = pic }) = pic

data TowerLockState =
    TowerLocked (PM.PMRef Monster)
  | TowerNonLocked
  deriving (Show)

data Tower = Tower
  { _damage    :: Int
  , _image     :: Picture
  , _range     :: Int
  , _towerCost :: Int
  , _lockState :: TowerLockState
  } deriving (Show)

data Monster = Monster
  { _moPicture   :: Picture
  , _speed       :: Speed
  , _currVec     :: (AbsolutePosition, TilePosition)
  , _vecIterator :: MonsterIterator
  , _totalHealth :: Int
  , _health      :: Int
  }

data Projectile = Projectile
  { _projectilePicture  :: Picture
  , _projectileSpeed    :: Speed
  , _projectileDamage   :: Damage
  , _projectilePosition :: AbsolutePosition
  , _projectileIterator :: ProjectileIterator
  }

instance Show Monster where
  show (Monster _pic speed currVec _it tH h) = "Monster { speed = " <> show speed <> ", currVec = " <> show currVec <> ", totalHealth = " <> show tH <> ", health = " <> show h <> "}"

translateImg :: (AbsolutePosition, Picture) -> Picture
translateImg (AbsolutePosition (x, y), pic) = Translate x y pic

healthBarWidth :: Fractional f => f
healthBarWidth = 30

healthBar :: Int -> Int -> Picture
healthBar totalHealth health =
  let
    remainingLife = healthBarWidth * (fromIntegral health / fromIntegral totalHealth)
    lostLife = healthBarWidth - remainingLife
    greenPart = Color green (rectangleSolid remainingLife 5)
    -- Nevermind about this calculation. It's about rectangle being drawn at origin.
    redPart = Color red (Translate (remainingLife / 2 + lostLife / 2) 0.0 (rectangleSolid lostLife 5))
  in
    Translate (- lostLife / 2) 0.0 $ greenPart <> redPart

instance HasPicture Monster where
  -- Fix the annotation. That type should be inferreble (maybe use fundeps?)
  getPicture (Monster pic _ (pos, _) _ tH h) =
      translateImg (second (+ 30)  `inside` pos, healthBar tH h)
    <> translateImg (pos, pic)

instance HasPicture Projectile where
  getPicture (Projectile pic _ _ pos _) = translateImg (pos, pic)

-- For any type that has picture, its PM.Map also has a picture
instance HasPicture a => HasPicture (PM.Map a) where
  getPicture = PM.elems >>> map getPicture >>> mconcat

instance HasPicture TileMap where
  getPicture = Map.assocs
           >>> map (second getPicture >>> first convertPos >>> translateImg)
           >>> mconcat

scalePos :: (Int, Int) -> (Float, Float)
scalePos = both *~ tileSize >>> both %~ fromIntegral

scaleWith :: Int -> (Int, Int) -> (Float, Float)
scaleWith magn = both *~ magn >>> both %~ fromIntegral

unscalePos :: (Float, Float) -> (Int, Int)
unscalePos = (both %~ floor) >>> (both %~ (`div` tileSize))

data PlayerInfo = PlayerInfo
  { _lives :: Int
  , _gold  :: Int
  }
makeLenses ''PlayerInfo

smallText :: String -> Picture
smallText = Scale 0.1 0.1 . Text

smallTexts :: [(String, String)] -> Picture
smallTexts [] = mempty
smallTexts ((desc, val):xs) =
  Scale 0.1 0.1 (Text (desc <> ": " <> val))
  <> Translate 0 (-20) (smallTexts xs)

instance HasPicture PlayerInfo where
  getPicture (PlayerInfo lives gold) =
    smallTexts [ ("Lives", show lives)
               , ("Gold", show gold)
               ]

makeLenses ''Tower
makeLenses ''Monster
makeLenses ''Projectile

makeLenses ''World

-- | Here is the algorithm:
--
-- From (3, 2) to (10, 15) we don't just want this particule to move with (1, 1) vector. Instead, we want to move with probabilities (7/20, 13/20). This way we'll achieve a natural movement flow.
moveTowards :: Int -> AbsolutePosition -> AbsolutePosition -> AbsolutePosition
moveTowards rand (AbsolutePosition (x, y)) (AbsolutePosition (tX, tY)) =
  AbsolutePosition (x + decide xP, y + decide yP)
  where
    xD = tX - x
    yD = tY - y
    sum = abs xD + abs yD
    xP = xD / sum
    yP = yD / sum
    decide :: Float -> Float
    decide t = if abs t * 100 > fromIntegral (rand `mod` 100) then signum t else 0


matchesTile :: AbsolutePosition -> Bool
matchesTile (AbsolutePosition (x, y)) =
  floor x `mod` tileSize == 0 && floor y `mod` tileSize == 0

distance :: AbsolutePosition -> AbsolutePosition -> Int
distance (AbsolutePosition (x1, y1)) (AbsolutePosition (x2, y2)) = floor $ sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

