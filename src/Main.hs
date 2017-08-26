{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Arrow                    (first, second, (&&&), (***), (>>>))
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
                         first scalePos .
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

newtype SW a = SW { runSW :: State World a }
  deriving (Functor, Applicative, Monad, MonadState World)

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
          modify f -- Actual event mutation
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

startShooting :: Position -> PM.PMRef MovingObject -> SW ()
startShooting pos target = do
  globalTime' <- gets _globalTime
  fireballPic <- use (assets . moAssets . fireball)
  let fireballObj = MovingObject fireballPic 50 (scalePos pos, (1, 0)) (projectile target)
      event = movingObjects %~ (PM.|>> Just fireballObj)
  schedEvents <>= Heap.singleton (Heap.Entry (globalTime' + 5) event)

updateIO :: Float -> World -> IO World
updateIO _ world = return $ execState (runSW update) world
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
eventHandlerIO ev world = return $ execState (runSW (eventHandler ev)) world

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
  let newEvents :: SchedEventHeap
      newEvents = Heap.fromList $
                    [0..(amount - 1)] &
                    traverse *~ interval &
                    traverse +~ globalTime' + fromMaybe 0 mbDelay &
                      traverse %~ (\time' -> Heap.Entry time' (movingObjects %~ (PM.|>> Just obj)))
  schedEvents <>= newEvents

--------------------------------------------------------------------------------

registerLevelEvents :: SW ()
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

nextLevel :: SW ()
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
    (execState (runSW nextLevel) $ initWorld assets)
    displayIO
    eventHandlerIO
    updateIO
