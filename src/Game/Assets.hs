{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Assets where

--------------------------------------------------------------------------------
import           Graphics.Gloss
import           Control.Lens
--------------------------------------------------------------------------------

data MOAssets = MOAssets
  { _fireball :: Picture
  , _centaur  :: Picture
  }

data GUIAssets = GUIAssets
  { _brick :: Picture
  }

data Assets = Assets
  { _grass      :: Picture
  , _closedDoor :: Picture
  , _stone      :: Picture
  , _openDoor   :: Picture
  , _cursor     :: Picture
  , _whiteTower :: Picture
  , _greenTower :: Picture
  , _moAssets   :: MOAssets
  , _guiAssets  :: GUIAssets
  }

makeLenses ''MOAssets
makeLenses ''GUIAssets
makeLenses ''Assets

genAssets :: IO Assets
genAssets = do
  _grass      <- loadBMP "assets/dc-misc/slot_eq.bmp"
  _closedDoor <- loadBMP "assets/dc-dngn/dngn_closed_door.bmp"
  _stone      <- loadBMP "assets/dc-dngn/floor/cobble_blood1.bmp"
  _openDoor   <- loadBMP "assets/dc-dngn/dngn_open_door.bmp"
  _cursor     <- loadBMP "assets/dc-misc/cursor.bmp"
  _whiteTower <- loadBMP "assets/dc-dngn/altars/dngn_altar_elyvilon.bmp"
  _greenTower <- loadBMP "assets/dc-dngn/altars/dngn_altar_jiyva02.bmp"
  _moAssets   <- do
    _fireball   <- loadBMP "assets/spells/fire/fireball.bmp"
    _centaur    <- loadBMP "assets/dc-mon/centaur.bmp"
    return MOAssets {..}
  _guiAssets  <- do
    _brick <- loadBMP "assets/dc-dngn/wall/brick_brown0.bmp"
    return GUIAssets {..}
  return Assets {..}

