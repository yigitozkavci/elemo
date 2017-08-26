{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TupleSections #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Arrow                    (first, second, (&&&), (***))
import           Control.Lens
import           Control.Lens.Operators
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Zipper                   (farthest)
import           Data.Fixed                       (mod')
import           Data.Foldable                    (toList)
import qualified Data.Heap                        as Heap
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.Monoid
import           Data.Tuple                       (swap)
import           Graphics.Gloss                   hiding (blank, display)
import           Graphics.Gloss.Data.ViewPort     (ViewPort)
import           Graphics.Gloss.Interface.IO.Game
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

displayIO :: World -> IO Picture
displayIO = return . display
    
display :: World -> Picture
display world = paintGUI (_assets world) (_guiState world)
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

tilegenLevel' :: State World ()
tilegenLevel' = do
  assets' <- use assets
  level' <- use level
  let (TilegenState levelPic' _ levelTileMap) = execTilegen assets' (tilegenLevel level')
  levelPic .= levelPic'
  wTileMap <>= levelTileMap

tilegenGUI' :: State World ()
tilegenGUI' = do
  assets' <- use assets
  guiState' <- use guiState
  let (TilegenState _ _ guiTileMap) = execTilegen assets' (tilegenGUI guiState')
  wTileMap <>= guiTileMap

update :: State World ()
update = do
  tilegenLevel'
  tilegenGUI'
  use builtTowers >>= (wTileMap <>=)
  movingObjects <~ (PM.mapM runMO =<< use movingObjects)
  towerShootings
  globalTime += 1
  modify $ farthest consumeSchedEvent

  where
    runMO :: Maybe MovingObject -> State World (Maybe MovingObject)
    runMO (Just (MovingObject pic speed vec f)) = do
      time <- use globalTime
      tileMap <- use wTileMap
      mos <- use movingObjects
      case f (time, tileMap, mos) pic speed vec of
        Nothing     -> return Nothing
        Just newVec -> return $ Just $ MovingObject pic speed newVec f
    runMO Nothing = return Nothing

    consumeSchedEvent :: World -> Maybe World
    consumeSchedEvent world = case Heap.viewMin (_schedEvents world) of
      Just (Heap.Entry time f, newHeap)
        | time == (world ^. globalTime) -> Just $ world & (schedEvents .~ newHeap)
                                                        & f
        | otherwise -> Nothing
      Nothing -> Nothing

    towerShootings :: State World ()
    towerShootings = do
      builtTowers' <- Map.assocs <$> gets _builtTowers
      newTowers <- Map.fromList <$> forM builtTowers' towerShooting
      builtTowers .= newTowers

    towerShooting :: (Position, UIObject) -> State World (Position, UIObject)
    towerShooting (pos, tower@(UITower (Tower dmg pic lockState))) = do
      movingObjects' <- gets _movingObjects
      case lockState of
        TowerNonLocked ->
          case PM.assocs movingObjects' of
            []               -> return (pos, tower) -- Tower non locked and there is no target
            ((moRef, _mo):_) -> do                  -- Lock the tower
              -- Register shoot event here
              startShooting pos moRef
              return (pos, UITower (Tower dmg pic (TowerLocked moRef)))
        TowerLocked moRef ->
          case PM.lookup moRef movingObjects' of
            Nothing -> return (pos, UITower (Tower dmg pic TowerNonLocked)) -- Target is lost
            Just _  -> return (pos, tower) -- Tower has a target and target is still alive

startShooting :: Position -> PM.PMRef MovingObject -> State World ()
startShooting pos target = do
  globalTime' <- gets _globalTime
  fireballPic <- use (assets . moAssets . fireball)
  let scaledPos = pos & both *~ tileSize
      fireballObj = MovingObject fireballPic 50 (scaledPos, (1, 0)) (projectile target)
      event = movingObjects %~ (PM.|>> Just fireballObj)
  schedEvents <>= Heap.singleton (Heap.Entry (globalTime' + 5) event)

updateIO :: Float -> World -> IO World
updateIO _ world = return $ execState update world
--------------------------------------------------------------------------------

moveTowards :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTowards (x, y) (tX, tY) = tupleSum (x, y) movVec
  where
    movVec = (tX - x, tY - y) & both %~ sig

projectile :: PM.PMRef MovingObject -> MovingVecIterator
projectile moRef (time, _, mos) pic speed ((x, y), dir) =
  if speedSync then
    case PM.lookup moRef mos of
      Nothing -> Nothing -- When target is lost, this projectile should also disappear
      Just target -> let ((targetX, targetY), _dir) = _currVec target
                     in
                       Just (moveTowards (x, y) (targetX, targetY), dir)
  else
    Just ((x, y), dir)
  where
    speedSync :: Bool
    speedSync = time `mod` (gameFreq `div` speed) == 0

tileFollower :: Picture -> MovingVecIterator
tileFollower tile (time, tileMap, _) _pic speed ((x, y), dir) =
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
    speedSync = time `mod` (gameFreq `div` speed) == 0

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
  -> State World ()
pushMovObjs mbDelay amount interval obj = do
  globalTime' <- use globalTime
  let newEvents :: SchedEventHeap
      newEvents = Heap.fromList $
                    [0..(amount - 1)] &
                    traverse *~ interval &
                    traverse +~ globalTime' + fromMaybe 0 mbDelay &
                      traverse %~ (\time' -> Heap.Entry time' (movingObjects %~ (PM.|>> Just obj)))
  schedEvents <>= newEvents

--------------------------------------------------------------------------------

registerLevelEvents :: State World ()
registerLevelEvents = do
  centaur <- use $ assets . moAssets . centaur
  fireball <- use $ assets . moAssets . fireball
  grass <- use $ assets . grass
  level <- use level
  case level of
    1 ->
      let centaur' = MovingObject
            { _moPicture   = centaur
            , _currVec     = ((28, 0), (1, 0))
            , _speed       = 50
            , _vecIterator = tileFollower grass
            }
          fireball' = MovingObject
            { _moPicture   = fireball
            , _currVec     = ((28, 0), (1, 0))
            , _speed       = 100
            , _vecIterator = tileFollower grass
            }
      in
        pushMovObjs (Just 2000) 5 500 centaur' >>
        pushMovObjs (Just 2000) 3 200 fireball'
              -- . registerNextLevel 15000 -- Go to next level after 15 secs. (disabled for now)
    other -> error $ "Events for level is not implemented: " <> show other
  -- where
  --   registerNextLevel :: Int -> World -> World
  --   registerNextLevel sec =
  --     schedEvents <>~ Heap.singleton (Heap.Entry (sec + _globalTime world) (level +~ 1))

nextLevel :: State World ()
nextLevel = do
  level += 1
  registerLevelEvents

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
    (execState nextLevel $ initWorld assets)
    displayIO
    eventHandler
    updateIO
