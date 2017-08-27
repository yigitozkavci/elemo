{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Tilegen where

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.Functor.Identity    (runIdentity)
import qualified Data.Map                 as Map
import           Data.Monoid
import qualified Data.Sequence            as Seq
import           Graphics.Gloss           hiding (blank)
import           Prelude                  hiding (Left, Right)
--------------------------------------------------------------------------------
import           Game.Assets
import           Game.Types
--------------------------------------------------------------------------------

data TilegenState = TilegenState
  { _picture :: Picture
  , _tAssets :: Assets
  , _tileMap :: TileMap
  }

makeLenses ''TilegenState

tilegenLevel :: Int -> TilegenM ()
tilegenLevel 1 = do
  assets <- gets _tAssets
  squareTiles (Floor True (assets ^. stone)) (1, 1) 9
  rectTiles (Floor False (assets ^. grass)) (1, 0) 10 1
  rectTiles (Floor False (assets ^. grass)) (10, 0) 1 11
  rectTiles (Floor False (assets ^. grass)) (0, 10) 11 1
  rectTiles (Floor False (assets ^. grass)) (0, 6) 1 5
  singleTile (Floor False (assets ^. openDoor)) (0, 0)
  singleTile (Floor False (assets ^. closedDoor)) (0, 5)
tilegenLevel level = error $ "Tilegen for level " <> show level <> " is not implemented."

paintGUI :: Assets -> GUIState -> Picture
paintGUI assets state =
  _picture $ execTilegen assets (tilegenGUI state)

tilegenGUI :: GUIState -> TilegenM ()
tilegenGUI state = do
  assets <- gets _tAssets
  rectTiles (Floor False (assets ^. guiAssets . brick)) (-1, -6) 1 3
  let positionedTowers = Seq.mapWithIndex (\i tower -> (i, tower)) (_guiTowers state)
  forM_ positionedTowers $ \(pos, tower) -> do
    singleTile (Floor False (assets ^. guiAssets . brick)) (pos, -4)
    singleTile (UITower tower) (pos, -5)
    singleTile (Floor False (assets ^. guiAssets . brick)) (pos, -6)

  rectTiles (Floor False (assets ^. guiAssets . brick)) (length (_guiTowers state), -6) 1 3


-- type TDM a = LoggingT (State TilegenState a)
newtype TilegenM a = TilegenM { runTilegenM :: State TilegenState a }
  deriving (Functor, Applicative, Monad, MonadState TilegenState)

execTilegen :: Assets -> TilegenM () -> TilegenState
execTilegen assets tilegen = runIdentity $ execStateT (runTilegenM tilegen) initState
  where
    initState = TilegenState
      { _picture = Blank
      , _tAssets  = assets
      , _tileMap = Map.empty
      }

squareTiles :: UIObject -> (Int, Int) -> Int -> TilegenM ()
squareTiles obj startPos sideLength = rectTiles obj startPos sideLength sideLength

singleTile :: UIObject -> (Int, Int) -> TilegenM ()
singleTile obj pos = do
  let (x, y) = pos & (both %~ fromIntegral) . (both *~ tileSize)
  picture <>= Translate x y (getPicture obj)
  tileMap %= Map.insert pos obj -- We insert non-scaled position

rectTiles :: UIObject -> (Int, Int) -> Int -> Int -> TilegenM ()
rectTiles obj (startX, startY) width height = do
  let coords = [ (x, y) | x <- [startX..(startX + width - 1)], y <- [startY..(startY + height - 1)] ]
  forM_ coords (singleTile obj)
