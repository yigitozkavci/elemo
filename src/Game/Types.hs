{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

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
import           Data.Fixed               (mod')
import qualified Data.Heap                as Heap
import           Data.Monoid              (mconcat, (<>))
import qualified Data.PreservedMap        as PM
import qualified Data.Sequence            as Seq
import qualified Data.Sequence.Queue      as Q
import qualified Data.Text                as T
import           Game.Assets
import           Game.Position
import           Game.Utils
import           Game.Direction
--------------------------------------------------------------------------------

data GUIState = GUIState
  { _guiTowers      :: Seq.Seq Tower
  , _guiTowerPosMap :: Map.Map (Int, Int) Tower
  }

initGUIState :: Assets -> GUIState
initGUIState assets = GUIState
  { _guiTowers = Seq.empty
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
  , _projectiles   :: PM.Map Projectile
  , _playerInfo    :: PlayerInfo
  , _alerts        :: Q.Queue T.Text
  }

newtype SW a = SW { runSW :: StateT World (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadState World, MonadLogger, MonadIO)

type GlobalTime = Int -- In miliseconds

type Speed = Int
type Damage = Int

class HasPicture m where
  getPicture :: m -> Picture

--------------------------------------------------------------------------------

type TileMap = Map.Map TilePosition UIObject

type MonsterIterator = Speed -> (AbsolutePosition, Direction) -> SW (Maybe (AbsolutePosition, Direction))

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
  , _canShoot  :: Bool
  } deriving (Show)

data Monster = Monster
  { _moPicture   :: Picture
  , _speed       :: Speed
  , _currVec     :: (AbsolutePosition, Direction)
  , _vecIterator :: MonsterIterator
  , _totalHealth :: Int
  , _health      :: Int
  , _goldYield   :: Int
  }

data Projectile = Projectile
  { _projectilePicture  :: Picture
  , _projectileSpeed    :: Speed
  , _projectileDamage   :: Damage
  , _projectilePosition :: AbsolutePosition
  , _projectileIterator :: ProjectileIterator
  }

-- | Functions are not showable, so we need custom instance.
instance Show Monster where
  show (Monster _pic speed currVec _it tH h gY) =
    "Monster {"
    <> "  speed = " <> show speed
    <> ", currVec = " <> show currVec
    <> ", totalHealth = " <> show tH
    <> ", health = " <> show h
    <> ", goldYield = " <> show gY
    <> "}"

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
  getPicture (Monster pic _ (pos, _) _ tH h _) =
      translateImg (second (+ 30)  `insidePos` pos, healthBar tH h)
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

moveTowards :: AbsolutePosition -> AbsolutePosition -> AbsolutePosition
moveTowards (AbsolutePosition (x, y)) (AbsolutePosition (tX, tY)) =
  AbsolutePosition (x + xP, y + yP)
  where
    xD = tX - x
    yD = tY - y
    sum = abs xD + abs yD
    xP = xD / sum
    yP = yD / sum

matchesTile :: AbsolutePosition -> Bool
matchesTile (AbsolutePosition (x, y)) =
  floor x `mod` tileSize == 0 && floor y `mod` tileSize == 0

distance :: AbsolutePosition -> AbsolutePosition -> Int
distance (AbsolutePosition (x1, y1)) (AbsolutePosition (x2, y2)) = floor $ sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2
