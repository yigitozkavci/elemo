module Game.Pathfinding where

import Game.Types
import Control.Lens
import Safe (headMay)
import Game.Position
import qualified Data.Map as Map

type Obsticle = AbsolutePosition

fetchObsticle :: AbsolutePosition -> AbsolutePosition -> SW (Maybe Obsticle)
fetchObsticle (AbsolutePosition start@(x1, y1)) (AbsolutePosition target@(x2, y2)) = do
  tileMap' <- use wTileMap
  let diff@(dX, dY) = (x2 - x1, y2 - y1)
      dirVec = diff & both //~ (dX + dY)
      testNodeVec = dirVec & both *~ tileSize
      testNodes = between start target testNodeVec
  return $ headMay $ filter (not . nodeAvailable tileMap') testNodes

between
  :: AbsolutePosition
  -- ^ Start
  -> AbsolutePosition
  -- ^ End
  -> AbsolutePosition
-- ^ Vector for progress (eg. (18.1, -9.0))
  -> [AbsolutePosition]
-- ^ Resulting intermediate nodes
between = undefined

nodeAvailable :: TileMap -> AbsolutePosition -> Bool
nodeAvailable tileMap' pos =
  case Map.lookup (convertPos pos) tileMap' of
    Just (UITower _) -> False -- There is a tower
    Just (Floor True _) -> False -- Floor that is placable on. Probably stone.
    Just (Floor False _) -> True -- Non-placable floor. Probably grass
    Nothing -> False -- There is no object at all. It's void.
