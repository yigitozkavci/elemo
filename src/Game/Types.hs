{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Types where

--------------------------------------------------------------------------------
import           Control.Arrow     ((>>>), first, second)
import           Control.Lens
import qualified Data.Map          as Map
import           Control.Monad.Logger     (LoggingT, MonadLogger,
                                           runStdoutLoggingT)
import           Control.Monad.State
import           Control.Monad.State.Lazy
import           Graphics.Gloss
import System.Random
--------------------------------------------------------------------------------
import           Data.Monoid       ((<>), mconcat)
import qualified Data.PreservedMap as PM
import qualified Data.Heap as Heap
import Game.Assets
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Sequence.Queue as Seq
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
type Position = (Int, Int)

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

newtype TilePosition = TilePosition (Int, Int)
  deriving (Show, Eq, Ord)

type TileMap = Map.Map TilePosition UIObject

type MonsterIterator = Speed -> (Position, Direction) -> SW (Maybe (Position, Direction))

type ProjectileIterator = (GlobalTime, PM.Map Monster) -> Speed -> Damage -> Position -> SW (Maybe Position)

data UIObject =
    Floor Bool Picture -- (Floor isPlacable picture)
  | UITower Tower
  deriving (Show)

instance HasPicture UIObject where
  getPicture (Floor _ pic) = pic
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
  , _currVec     :: (Position, Direction)
  , _vecIterator :: MonsterIterator
  , _totalHealth :: Int
  , _health      :: Int
  }

data Projectile = Projectile
  { _projectilePicture :: Picture
  , _projectileSpeed :: Speed
  , _projectileDamage :: Damage
  , _projectilePosition :: Position
  , _projectileIterator :: ProjectileIterator
  }

instance Show Monster where
  show (Monster _pic speed currVec _it tH h) = "Monster { speed = " <> show speed <> ", currVec = " <> show currVec <> ", totalHealth = " <> show tH <> ", health = " <> show h <> "}"

translateImg :: (Position, Picture) -> Picture
translateImg ((x, y), pic) = Translate (fromIntegral x) (fromIntegral y) pic

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
  getPicture (Monster pic _ (pos, _) _ tH h) =
       translateImg (second (+30) pos, healthBar tH h)
    <> translateImg (pos, pic)

instance HasPicture Projectile where
  getPicture (Projectile pic _ _ pos _) = translateImg (pos, pic)

-- For any type that has picture, its PM.Map also has a picture
instance HasPicture a => HasPicture (PM.Map a) where
  getPicture = PM.elems >>> map getPicture >>> mconcat

instance HasPicture TileMap where
  getPicture = Map.assocs
           >>> map (second getPicture >>> first scalePos >>> translateImg)
           >>> mconcat

scalePos :: TilePosition -> (Int, Int)
scalePos (TilePosition pos) = pos & both *~ tileSize

unscalePos :: (Int, Int) -> TilePosition
unscalePos pos = TilePosition $ pos & both %~ (`div` tileSize)

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
