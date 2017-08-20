{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE EmptyDataDecls #-}

module Main where

--------------------------------------------------------------------------------
import           Graphics.Gloss hiding        (blank)
import           Graphics.Gloss.Interface.IO.Game
import           Data.Monoid
import           Graphics.Gloss.Data.ViewPort (ViewPort)
import           Prelude                      hiding (Left, Right)
import           Control.Lens
import           Control.Lens.Operators
import           Control.Arrow                ((***), (&&&), first, second)
import qualified Data.Map                     as Map
import           Data.Maybe                   (mapMaybe, fromMaybe)
import           Data.Tuple                   (swap)
import           Control.Monad
import           Data.Fixed                   (mod')
import qualified Data.Heap                    as Heap
import           Data.Foldable                (toList)
import           Control.Zipper               (farthest)
--------------------------------------------------------------------------------
import           Utils
import           Tilegen                      hiding (_tileMap) -- TODO: fix
import           Assets
import           World
import           Types
import           GUI
--------------------------------------------------------------------------------

tilemapToPicture :: TileMap -> Picture
tilemapToPicture = mconcat .
                   map ( translateImg .
                         first (both *~ tileSize) .
                         second getPicture
                       ) .
                   Map.assocs

adjustPosToIndex :: (Float, Float) -> (Int, Int)
adjustPosToIndex = both %~ ( floor .
                             (/ fromIntegral tileSize) .
                             (+ (fromIntegral tileSize / 2))
                           )

translateImg :: (Position, Picture) -> Picture
translateImg ((x, y), pic) = Translate (fromIntegral x) (fromIntegral y) pic

gDisplay :: World -> IO Picture
gDisplay world =
  return $ paintGUI (_assets world) (_guiState world)
        <> _levelPic world
        <> movingObjs
        <> mouseCursor
        <> builtTowers'
  where
    mouseCursor :: Picture
    mouseCursor = case world ^. selectorState of
      MouseFree -> Blank
      SelectedItem obj -> let (mX, mY) = world ^. mousePos & adjustMouseToTile in
                          Translate mX mY (_image obj)

    adjustMouseToTile :: (Float, Float) -> (Float, Float)
    adjustMouseToTile = both %~ ( fromIntegral .
                                  (* tileSize) .
                                  floor .
                                  (/ fromIntegral tileSize) .
                                  (+ (fromIntegral tileSize / 2))
                                )

    movingObjs :: Picture
    movingObjs = mconcat . map translateImg $ world & toListOf (movingObjects . traverse . imgAndPos)

    imgAndPos :: Getter MovingObject (Position, Picture)
    imgAndPos = runGetter $ (,) <$> Getter (currVec . _1) <*> Getter moPicture

    builtTowers' :: Picture
    builtTowers' = tilemapToPicture (_builtTowers world)
--------------------------------------------------------------------------------

gUpdate :: Float -> World -> IO World
gUpdate _ world =
  let (TilegenState levelPic' _ levelTileMap) = execTilegen (_assets world) (tilegenLevel (_level world))
      (TilegenState _ _ guiTileMap)          = execTilegen (_assets world) (tilegenGUI (_guiState world))
  in
    return $ world & (levelPic .~ levelPic')
                   . (wTileMap .~ (_builtTowers world <> levelTileMap <> guiTileMap))
                   . (movingObjects %~ modifyMOs)
                   . (globalTime %~ (+ 1))
                   . farthest consumeSchedEvent -- Consume as much event as possible.
  where
    modifyMOs :: [MovingObject] -> [MovingObject]
    modifyMOs [] = []
    modifyMOs (MovingObject pic speed vec f:xs) =
      case f world pic speed vec of
        Nothing -> modifyMOs xs
        Just newVec -> MovingObject pic speed newVec f : modifyMOs xs

    consumeSchedEvent :: World -> Maybe World
    consumeSchedEvent world' = case Heap.viewMin (_schedEvents world') of
      Just (Heap.Entry time f, newHeap)
        | time == (world ^. globalTime) -> Just $ world' & (schedEvents .~ newHeap) . f
        | otherwise -> Nothing
      Nothing -> Nothing

--------------------------------------------------------------------------------

projectile :: (World -> Maybe MovingObject) -> MovingVecIterator
projectile getObj world pic speec ((x, y), dir) =
  case getObj world of
    Nothing -> Nothing -- When target is lost, this projectile should also disappear
    Just movObj -> let ((targetX, targetY), dir) = _currVec movObj
                   in
                     Just ((x, y), dir)

tileFollower :: Picture -> MovingVecIterator
tileFollower tile world _pic speed ((x, y), dir) =
  if speedSync then
    if x `mod` tileSize == 0 && y `mod` tileSize == 0 then
      case availablePositions of
        [] -> Nothing
        (x:_) -> Just x
    else
      Just (tupleSum (x, y) dir, dir)
  else
    Just ((x, y), dir)
  where
    speedSync :: Bool
    speedSync = _globalTime world `mod` (gameFreq `div` speed) == 0

    availablePositions =
      -- After filtering, direction addings must be cut. We don't want `tileSize` amount
      -- of movement, afterall.
      map (\(pos, dir) -> (tupleSum pos (mapTuple (* (-tileSize + 1)) dir), dir)) .

      -- Accept only positions that are matching to the given tile type from points of interest
      filter (\((xPos, yPos), _) ->
        let indexPos = (xPos `div` tileSize, yPos `div` tileSize) in
        case Map.lookup indexPos (_wTileMap world) of
          Just obj
            | getPicture obj == tile -> True
          _ -> False
      ) $

      posOfIntr

    -- Positions of interest. For position (28, 56) and direction (1, 0),
    -- possible positions will be [(56, 56), (28, 84), (28, 28)]
    --
    -- x: current location
    -- .: positions of interest
    -- ========================
    -- o.oo
    -- ox.o
    -- o.oo
    posOfIntr :: [(Position, Direction)]
    posOfIntr = map (((+x) *** (+y)) &&& mapTuple (`div` tileSize)) $
      map (mapTuple (* tileSize))
        [ dir 
        , swap dir
        , both *~ -1 $ swap dir
        ]

--------------------------------------------------------------------------------

guiClick :: (Float, Float) -> World -> Maybe UIObject
guiClick pos world = Map.lookup (adjustPosToIndex pos) (_wTileMap world)

eventHandler :: Event -> World -> IO World
eventHandler ev world =
  case ev of
    EventMotion pos -> return $ world & mousePos .~ pos
    EventKey (MouseButton LeftButton) Down _modifiers pos ->
      return $ case _selectorState world of
        MouseFree -> 
          case guiClick pos world of
            Just (UITower tower) -> world & selectorState .~ SelectedItem tower
            _ -> world
        SelectedItem tower ->
          case guiClick pos world of
            Just (Floor True _) -> world & (builtTowers %~ Map.insert (adjustPosToIndex pos) (UITower tower))
                                         . (selectorState .~ MouseFree)
            _ -> world
    EventKey (MouseButton RightButton) Down _modifiers _pos ->
      return $ world & selectorState .~ MouseFree
    _ -> return world

pushMovObjs
  :: Maybe Int
    -- ^ Base delay (optional)
  -> Int
    -- ^ Amount of objs
  -> Int
    -- ^ Interval
  -> MovingObject
    -- ^ Type of objs
  -> World
    -- ^ Old world
  -> World
    -- ^ New world
pushMovObjs mbDelay amount interval obj world =
  world & schedEvents %~ (`Heap.union` newEvents)
  where
    newEvents :: SchedEventHeap
    newEvents = Heap.fromList $
                map ( (\time' -> Heap.Entry time' (movingObjects %~ (++ [obj]))) .
                      (+ fromMaybe 0 mbDelay) .
                      (+ _globalTime world) .
                      (* interval)
                    ) [0..(amount - 1)]

--------------------------------------------------------------------------------

registerLevelEvents :: World -> World
registerLevelEvents world =
  case _level world of
    1 -> 
      let centaur' = MovingObject
            { _moPicture   = world ^. assets . moAssets . centaur
            , _currVec     = ((28, 0), (1, 0))
            , _speed       = 50
            , _vecIterator = tileFollower (world ^. assets . grass)
            }
          fireball' = MovingObject
            { _moPicture   = world ^. assets . moAssets . fireball
            , _currVec     = ((28, 0), (1, 0))
            , _speed       = 100
            , _vecIterator = tileFollower (world ^. assets . grass)
            }
      in
        world & pushMovObjs (Just 2000) 5 500 centaur'
              . pushMovObjs (Just 2000) 3 200 fireball'
              -- . registerNextLevel 15000 -- Go to next level after 15 secs. (disabled for now)
    other -> error $ "Events for level is not implemented: " <> show other
  where
    registerNextLevel :: Int -> World -> World
    registerNextLevel sec =
      schedEvents %~ (Heap.Entry (sec + _globalTime world) (level %~ (+1)) `Heap.insert`)

nextLevel :: World -> World
nextLevel world =
  world & registerLevelEvents
        . (level %~ (+1))

initWorld :: Assets -> World
initWorld assets = World
  { _level         = 0
  , _levelPic      = Blank
  , _movingObjects = []
  , _wTileMap      = Map.empty
  , _globalTime    = 0
  , _schedEvents   = Heap.empty
  , _selectorState = MouseFree
  , _mousePos      = (0.0, 0.0)
  , _guiState      = initGUIState assets
  , _assets        = assets
  , _builtTowers   = Map.empty
  }

gameFreq :: Int
gameFreq = 1000

main :: IO ()
main = do
  assets <- genAssets
  playIO
    (InWindow "Nice Window" (700, 700) (0, 0))
    white
    gameFreq
    (nextLevel $ initWorld assets)
    gDisplay
    eventHandler
    gUpdate
