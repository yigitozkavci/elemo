{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Arrow                    (first, second, (&&&), (***),
                                                   (>>>))
import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad
import           Control.Monad.Logger             (runStdoutLoggingT)
import           Control.Monad.Logger.CallStack   (logInfo)
import           Control.Monad.State              (execStateT, gets, modify)
import qualified Data.Heap                        as Heap
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.Monoid                      (mempty, (<>))
import           Data.Tuple                       (swap)
import           Graphics.Gloss                   hiding (blank, display)
import           Graphics.Gloss.Interface.IO.Game
import           System.Random                    (StdGen, getStdGen, next)
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
tilemapToPicture =
      Map.assocs
  >>> map ( second getPicture
        >>> first scalePos
        >>> translateImg
      )
  >>> mconcat

adjustPosToIndex :: (Float, Float) -> (Int, Int)
adjustPosToIndex = over both $
      (+ (fromIntegral tileSize / 2))
  >>> (/ fromIntegral tileSize)
  >>> floor

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
    adjustMouseToTile = over both $
            (+ (fromIntegral tileSize / 2))
        >>> (/ fromIntegral tileSize)
        >>> floor
        >>> (* tileSize)
        >>> fromIntegral

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
        Just mo ->
          Line [scalePos pos & both %~ fromIntegral, fst (_currVec mo) & both %~ fromIntegral]
        Nothing -> mempty -- This is temporary second until tower finds a new target.
    towerLocking _ = mempty

--------------------------------------------------------------------------------

tilegenLevel' :: SW ()
tilegenLevel' = do
  assets' <- use assets
  level' <- use level
  let (TilegenState levelPic' _ levelTileMap) = execTilegen assets' (tilegenLevel level')
  levelPic .= levelPic'
  wTileMap <>= levelTileMap

tilegenGUI' :: SW ()
tilegenGUI' = do
  assets' <- use assets
  guiState' <- use guiState
  let (TilegenState _ _ guiTileMap) = execTilegen assets' (tilegenGUI guiState')
  wTileMap <>= guiTileMap

update :: SW ()
update = do
  tilegenLevel'
  tilegenGUI'
  use builtTowers >>= (wTileMap <>=)
  movingObjects <~ (PM.mapM runMO =<< use movingObjects)
  towerShootings
  globalTime += 1
  consumeSchedEvents

  where
    runMO :: Maybe MovingObject -> SW (Maybe MovingObject)
    runMO (Just (MovingObject pic speed vec f)) = do
      time <- use globalTime
      tileMap <- use wTileMap
      mos <- use movingObjects
      case f (time, tileMap, mos) pic speed vec of
        Nothing     -> return Nothing
        Just newVec -> return $ Just $ MovingObject pic speed newVec f
    runMO Nothing = return Nothing

    consumeSchedEvents :: SW ()
    consumeSchedEvents = do
      globalTime' <- use globalTime
      ev <- Heap.viewMin <$> use schedEvents
      forM_ ev $ \(Heap.Entry time f, newHeap) ->
        when (time == globalTime') $ do
          schedEvents .= newHeap
          f -- Actual event mutation
          consumeSchedEvents

    towerShootings :: SW ()
    towerShootings = do
      builtTowers' <- Map.assocs <$> gets _builtTowers
      newTowers <- Map.fromList <$> forM builtTowers' towerShooting
      builtTowers .= newTowers

    towerShooting :: (Position, UIObject) -> SW (Position, UIObject)
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

scalePos :: Position -> Position
scalePos = both *~ tileSize

unscalePos :: Position -> Position
unscalePos = both %~ (`div` tileSize)

addEvent :: Int -> SW () -> SW ()
addEvent time' ev = do
  globalTime' <- use globalTime
  schedEvents <>= Heap.singleton (Heap.Entry (globalTime' + time') ev)

getRandom :: SW Int
getRandom = do
  gen <- use randGen
  let (val, newGen) = next gen
  randGen .= newGen
  return val

startShooting :: Position -> PM.PMRef MovingObject -> SW ()
startShooting pos target = do
  globalTime' <- gets _globalTime
  fireballPic <- use (assets . moAssets . fireball)
  let fireballObj = MovingObject fireballPic 150 (scalePos pos, (1, 0)) (projectile target)
      event = do
        movingObjects %|>>= Just fireballObj
        addEvent 2000 event
  addEvent 500 event

updateIO :: Float -> World -> IO World
updateIO _ world = runStdoutLoggingT $ flip execStateT world $ runSW update

--------------------------------------------------------------------------------

speedSync :: Int -> Int -> Bool
speedSync time speed = time `mod` (gameFreq `div` speed) == 0

-- TODO: Projectile-type moving objects don't need `dir` parameter. Maybe remove it?
projectile :: PM.PMRef MovingObject -> MovingVecIterator
projectile moRef (time, _, mos) pic speed (pos@(x, y), dir) =
  if speedSync time speed then
    case PM.lookup moRef mos of
      Nothing -> Nothing -- When target is lost, this projectile should also disappear
      Just target ->
        let (targetPos@(targetX, targetY), _dir) = _currVec target in
        if distance pos targetPos < 5
          then
            Nothing -- Decrease health of the target by tower damage here.
          else
            Just (moveTowards pos targetPos, dir)
  else
    Just (pos, dir)

tileFollower :: Picture -> MovingVecIterator
tileFollower tile (time, tileMap, _) _pic speed ((x, y), dir) =
  if speedSync time speed then
    if x `mod` tileSize == 0 && y `mod` tileSize == 0 then
      case availablePositions of
        []    -> Nothing
        (x:_) -> Just x
    else
      Just (tupleSum (x, y) dir, dir)
  else
    Just ((x, y), dir)
  where
    availablePositions =
        -- Accept only positions that are matching to the given tile type from points of interest
        filter (\(pos', _) ->
          case Map.lookup (unscalePos pos') tileMap of
            Just obj
              | getPicture obj == tile -> True
            _ -> False
        )
        -- After filtering, direction addings must be cut. We don't want `tileSize` amount
        -- of movement, afterall.
        >>> map (\(pos, dir) -> (tupleSum pos (mapTuple (* (-tileSize + 1)) dir), dir))
        $ posOfIntr

    -- Positions of interest. For position (28, 56) and direction (1, 0),
    -- possible positions will be [(56, 56), (28, 84), (28, 28)]
    --
    -- x: current location
    -- .: positions of interest
    -- ========================
    -- o.oo
    -- ox.o
    -- o.oo
    --
    -- For arrow computation,  left computes positions, right computes directions
    posOfIntr :: [(Position, Direction)]
    posOfIntr =
      map (scalePos >>> ((+x) *** (+y)) &&& unscalePos)
        [ dir
        , swap dir
        , dir & (swap >>> both *~ -1)
        ]

--------------------------------------------------------------------------------

guiClick :: (Float, Float) -> SW (Maybe UIObject)
guiClick pos = Map.lookup (adjustPosToIndex pos) <$> use wTileMap

eventHandler :: Event -> SW ()
eventHandler = \case
  EventMotion pos ->
    mousePos .= pos
  EventKey (MouseButton LeftButton) Down _modifiers pos ->
    use selectorState >>= \case
      MouseFree ->
        guiClick pos >>= \case
          Just (UITower tower) -> selectorState .= SelectedItem tower
          _ -> return ()
      SelectedItem tower ->
        guiClick pos >>= \case
          Just (Floor True _) -> do
              builtTowers %= Map.insert (adjustPosToIndex pos) (UITower tower)
              selectorState .= MouseFree
          _ -> return ()
  EventKey (MouseButton RightButton) Down _modifiers _pos ->
    selectorState .= MouseFree
  _ -> return ()

eventHandlerIO :: Event -> World -> IO World
eventHandlerIO ev world =
  runStdoutLoggingT $ execStateT (runSW (eventHandler ev)) world

pushMovObjs
  :: Maybe Int
    -- ^ Base delay (optional)
  -> Int
    -- ^ Amount of objs
  -> Int
    -- ^ Interval
  -> MovingObject
    -- ^ Type of objs
  -> SW ()
pushMovObjs mbDelay amount interval obj = do
  globalTime' <- use globalTime
  logInfo "Pushing moving objects!"
  let newEvents :: SchedEventHeap
      newEvents = Heap.fromList $
                    [0..(amount - 1)] &
                    traverse *~ interval &
                      traverse +~ globalTime' + fromMaybe 0 mbDelay &
                      traverse %~ (\time' -> Heap.Entry time' (movingObjects %|>>= Just obj))
  schedEvents <>= newEvents

--------------------------------------------------------------------------------

getFireball :: SW MovingObject
getFireball = do
  pic <- use $ assets . moAssets . fireball
  grass <- use $ assets . grass
  return MovingObject
    { _moPicture   = pic
    , _currVec     = ((28, 0), (1, 0))
    , _speed       = 50
    , _vecIterator = tileFollower grass
    }

getCentaur :: SW MovingObject
getCentaur = do
  pic <- use $ assets . moAssets . centaur
  grass <- use $ assets . grass
  return MovingObject
    { _moPicture   = pic
    , _currVec     = ((28, 0), (1, 0))
    , _speed       = 50
    , _vecIterator = tileFollower grass
    }

registerLevelEvents :: SW ()
registerLevelEvents = do
  fireball <- use $ assets . moAssets . fireball
  grass <- use $ assets . grass
  level <- use level
  case level of
    1 -> do
      pushMovObjs (Just 2000) 5 500 =<< getCentaur
      -- pushMovObjs (Just 2000) 5 200 =<< getFireball
      -- registerNextLevel 15000 -- Go to next level after 15 secs. (disabled for now)
    other -> error $ "Events for level is not implemented: " <> show other

registerNextLevel :: Int -> SW ()
registerNextLevel sec = do
  globalTime' <- use globalTime
  addEvent sec (level += 1)

nextLevel :: SW ()
nextLevel = do
  level += 1
  logInfo "whoa!"
  registerLevelEvents

initWorld :: Assets -> StdGen -> World
initWorld assets randGen = World
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
  , _randGen       = randGen
  }

gameFreq :: Int
gameFreq = 1000

main :: IO ()
main = do
  assets <- genAssets
  randGen <- getStdGen
  initWorld' <- runStdoutLoggingT $ flip execStateT (initWorld assets randGen) $ runSW nextLevel
  playIO
    (InWindow "Nice Window" (700, 700) (0, 0))
    white
    gameFreq
    initWorld'
    displayIO
    eventHandlerIO
    updateIO
