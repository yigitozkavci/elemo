module Game.Direction where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import           Game.Position
--------------------------------------------------------------------------------

data Direction =
    DRight
  | DLeft
  | DUp
  | DDown
  deriving (Show, Eq, Ord)

serializeDir :: Direction -> TilePosition
serializeDir = TilePosition . go
  where
    go DRight = (1, 0)
    go DDown  = (0, -1)
    go DLeft  = (-1, 0)
    go DUp    = (0, 1)

-- Arrow-shaped direction list. Used for pathfinding.
arrowDir :: Direction -> [Direction]
arrowDir DRight = [DUp,    DRight, DDown]
arrowDir DDown  = [DRight, DDown,  DLeft]
arrowDir DLeft  = [DDown,  DLeft,  DUp]
arrowDir DUp    = [DLeft,  DUp,    DRight]
