{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Tilegen where

--------------------------------------------------------------------------------
import           Data.Functor.Identity    (runIdentity)
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Prelude                  hiding (Left, Right)
import           Data.Monoid
import           Graphics.Gloss           hiding (blank)
import qualified Data.Map                 as Map
import           Control.Lens
import           Control.Lens.Operators
--------------------------------------------------------------------------------
import           Game.Assets
import           Game.Types
--------------------------------------------------------------------------------

data TilegenState = TilegenState
  { _picture :: Picture
  , _tAssets  :: Assets
  , _tileMap :: TileMap
  }

makeLenses ''TilegenState

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

--------------------------------------------------------------------------------

