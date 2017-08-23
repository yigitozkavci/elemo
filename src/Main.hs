{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE EmptyDataDecls #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Arrow                    (first, second, (&&&), (***))
import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad
import           Control.Zipper                   (farthest)
import           Data.Fixed                       (mod')
import           Data.Foldable                    (toList)
import qualified Data.Heap                        as Heap
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.Monoid
import           Data.Tuple                       (swap)
import           Graphics.Gloss                   hiding (blank)
import           Graphics.Gloss.Data.ViewPort     (ViewPort)
import           Graphics.Gloss.Interface.IO.Game
import           Prelude                          hiding (Left, Right)
--------------------------------------------------------------------------------
import qualified Data.PreservedMap                as PM
import           Game.Assets
import           Game.GUI
import           Game.Tilegen                     hiding (_tileMap)
import           Game.Types
import           Game.Utils
import           Game.World
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
gDisplay world = do
  -- print $ _movingObjects world
  return $ paintGUI (_assets world) (_guiState world)
        <> _levelPic world
        <> movingObjs
        <> mouseCursor
        <> builtTowers'
        <> towerLockings
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
    movingObjs = mconcat . map (translateImg . imgAndPos) . PM.elems $ _movingObjects world

    imgAndPos :: MovingObject -> (Position, Picture)
    imgAndPos mo = (fst (_currVec mo), _moPicture mo)

    builtTowers' :: Picture
    builtTowers' = tilemapToPicture (_builtTowers world)

    towerLockings :: Picture
    towerLockings = mconcat $ map towerLocking $ Map.assocs (_builtTowers world)

    towerLocking :: (Position, UIObject) -> Picture
    towerLocking (pos, UITower (Tower _ pic (TowerLocked moRef))) =
      case PM.lookup moRef (_movingObjects world) of
        Just mo -> Line [pos & both %~ (fromIntegral . (* tileSize)), fst (_currVec mo) & both %~ fromIntegral]
        Nothing -> mempty -- This is temporary second until tower finds a new target.
    towerLocking _ = mempty

--------------------------------------------------------------------------------

gUpdate :: Float -> World -> IO World
gUpdate _ world =
  let (TilegenState levelPic' _ levelTileMap) = execTilegen (_assets world) (tilegenLevel (_level world))
      (TilegenState _ _ guiTileMap)          = execTilegen (_assets world) (tilegenGUI (_guiState world))
  in
    return $ world & (levelPic .~ levelPic')
                   & (wTileMap .~ (_builtTowers world <> levelTileMap <> guiTileMap))
                   & (movingObjects %~ modifyMOs)
                   & handleTowerShooting
                   & (globalTime +~ 1)
                   & farthest consumeSchedEvent -- Consume as much event as possible.
  where
    modifyMOs :: PM.Map MovingObject -> PM.Map MovingObject
    modifyMOs = PM.map $ \mo -> case mo of
      Just (MovingObject pic speed vec f) ->
        case f (_globalTime world, _wTileMap world) pic speed vec of
          Nothing     -> Nothing
          Just newVec -> Just (MovingObject pic speed newVec f)
      Nothing -> Nothing

    consumeSchedEvent :: World -> Maybe World
    consumeSchedEvent world = case Heap.viewMin (_schedEvents world) of
      Just (Heap.Entry time f, newHeap)
        | time == (world ^. globalTime) -> Just $ world & (schedEvents .~ newHeap)
                                                        & f
        | otherwise -> Nothing
      Nothing -> Nothing

    handleTowerShooting :: World -> World
    handleTowerShooting world =
      let eventedTowers :: Map.Map Position (UIObject, [SchedEvent]) =
            Map.map (\uiObj ->
              case uiObj of
                UITower tower -> let (tower', events) = handleTowerShooting' world tower in
                  (UITower tower', events)
                other -> (other, [])
            )
            (_builtTowers world)
          newBuiltTowers = Map.map fst eventedTowers
          events = Heap.fromList $ concatMap snd $ Map.elems eventedTowers
      in
      world & builtTowers .~ newBuiltTowers
            & schedEvents <>~ events

    handleTowerShooting' :: World -> Tower -> (Tower, [SchedEvent])
    handleTowerShooting' world (Tower dmg pic TowerNonLocked) =
      case PM.assocs (_movingObjects world) of
        [] -> (Tower dmg pic TowerNonLocked, [])
        ((moRef, _mo):_) -> (Tower dmg pic (TowerLocked moRef), []) -- TODO: Emit next shoot event here
    handleTowerShooting' world (Tower dmg pic (TowerLocked moRef)) =
      case PM.lookup moRef (_movingObjects world) of
        Just _ -> (Tower dmg pic (TowerLocked moRef), [])
        Nothing -> (Tower dmg pic TowerNonLocked, []) -- Moving object is lost, unlock it
--------------------------------------------------------------------------------

moveTowards :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTowards (x, y) (tX, tY) = tupleSum (x, y) movVec
  where
    movVec = (tX - x, tY - y) & both %~ sig

projectile :: ((GlobalTime, TileMap) -> Maybe MovingObject) -> MovingVecIterator
projectile getTarget worldInfo pic speec ((x, y), dir) =
  case getTarget worldInfo of
    Nothing -> Nothing -- When target is lost, this projectile should also disappear
    Just target -> let ((targetX, targetY), _dir) = _currVec target
                   in
                     Just (moveTowards (x, y) (targetX, targetY), dir)

tileFollower :: Picture -> MovingVecIterator
tileFollower tile (globalTime, tileMap) _pic speed ((x, y), dir) =
  if speedSync then
    if x `mod` tileSize == 0 && y `mod` tileSize == 0 then
      case availablePositions of
        []    -> Nothing
        (x:_) -> Just x
    else
      Just (tupleSum (x, y) dir, dir)
  else
    Just ((x, y), dir)
  where
    speedSync :: Bool
    speedSync = globalTime `mod` (gameFreq `div` speed) == 0

    availablePositions =
      -- After filtering, direction addings must be cut. We don't want `tileSize` amount
      -- of movement, afterall.
      map (\(pos, dir) -> (tupleSum pos (mapTuple (* (-tileSize + 1)) dir), dir)) .

      -- Accept only positions that are matching to the given tile type from points of interest
      filter (\((xPos, yPos), _) ->
        let indexPos = (xPos `div` tileSize, yPos `div` tileSize) in
        case Map.lookup indexPos tileMap of
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
                                         & (selectorState .~ MouseFree)
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
pushMovObjs mbDelay amount interval obj world = world & schedEvents <>~ newEvents
  where
    newEvents :: SchedEventHeap
    newEvents = Heap.fromList $
                  [0..(amount - 1)] &
                    traverse *~ interval &
                    traverse +~ _globalTime world  + fromMaybe 0 mbDelay &
                    traverse %~ (\time' -> Heap.Entry time' (movingObjects %~ (PM.|>> obj)))

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
      schedEvents <>~ Heap.singleton (Heap.Entry (sec + _globalTime world) (level +~ 1))

nextLevel :: World -> World
nextLevel = registerLevelEvents . (level +~ 1)

initWorld :: Assets -> World
initWorld assets = World
  { _level         = 0
  , _levelPic      = Blank
  , _movingObjects = PM.empty
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
