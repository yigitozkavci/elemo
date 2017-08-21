{-# LANGUAGE TemplateHaskell #-}

module Game.World where

--------------------------------------------------------------------------------
import           Control.Lens
import           Graphics.Gloss
import qualified Data.Heap                as Heap
import qualified Data.Map                 as Map
import           Data.Monoid              ((<>))
import           Control.Monad.State.Lazy
--------------------------------------------------------------------------------
import           Game.Assets
import           Game.Tilegen
import           Game.Types
import           Game.GUI
import qualified Data.PreservedMap        as PM
--------------------------------------------------------------------------------

data SelectorState =
    MouseFree
  | SelectedItem Tower

type SchedEventHeap = Heap.Heap (Heap.Entry GlobalTime (World -> World))

data World = World
  { _level         :: Int
  , _levelPic      :: Picture
  , _movingObjects :: PM.Map MovingObject
  , _wTileMap      :: TileMap
  , _globalTime    :: GlobalTime -- | In milisecs
  , _schedEvents   :: SchedEventHeap -- | Events can mutate the whole world, but
                                     -- we entrust them not to modify @_schedEvents@. That can mess things up.
  , _selectorState :: SelectorState
  , _mousePos      :: (Float, Float)
  , _guiState      :: GUIState
  , _assets        :: Assets
  , _builtTowers   :: TileMap
  }

makeLenses ''World
makeLenses ''MovingObject

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
