module GUI where

import qualified Data.Sequence as Seq
import           Graphics.Gloss
import           Assets
import           Control.Lens.Operators
import           Tilegen
import           Data.Foldable          (forM_)
import           Types
import qualified Data.Map      as Map
import Control.Monad.State.Lazy

data GUIState = GUIState
  { _guiTowers :: Seq.Seq Tower
  , _guiTowerPosMap :: Map.Map (Int, Int) Tower
  }

initGUIState :: Assets -> GUIState
initGUIState assets = GUIState
  { _guiTowers = Seq.fromList
    [ Tower 10 (_whiteTower assets)
    , Tower 20 (_greenTower assets)
    ]
  , _guiTowerPosMap = Map.empty
  }

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
