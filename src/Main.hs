{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Applicative.Extra
import           Control.Arrow                    (first, second, (&&&), (***),
                                                   (>>>))
import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad
import           Control.Monad.Logger             (runStdoutLoggingT)
import           Control.Monad.Logger.CallStack   (logInfo)
import           Control.Monad.State              (execStateT, gets, modify)
import qualified Data.Heap                        as Heap
import           Data.List                        (sortBy)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust, mapMaybe)
import           Data.Monoid                      (mempty, (<>))
import qualified Data.Sequence.Queue              as Q
import qualified Data.Text                        as T
import           Data.Tuple                       (swap)
import           Graphics.Gloss                   hiding (blank, display)
import           Graphics.Gloss.Interface.IO.Game
import           Safe                             (headMay)
import           System.Random                    (StdGen, getStdGen, next)
--------------------------------------------------------------------------------
import qualified Data.PreservedMap                as PM
import           Game.Assets
import           Game.Tilegen                     hiding (_tileMap)
import           Game.Types
import           Game.Utils
--------------------------------------------------------------------------------

adjustPosToIndex :: (Float, Float) -> (Int, Int)
adjustPosToIndex = over both $
      (+ (fromIntegral tileSize / 2))
  >>> (/ fromIntegral tileSize)
  >>> floor

displayIO :: World -> IO Picture
displayIO = return . display

display :: World -> Picture
display world = paintGUI (world ^. assets) (world ^. guiState)
             <> _levelPic world
             <> getPicture (world ^. monsters)
             <> getPicture (world ^. projectiles)
             <> mouseCursor
             <> getPicture (world ^. builtTowers)
             <> towerLockings
             <> towerRanges
             <> Translate (-300) 300 (getPicture (world ^. playerInfo))
             <> paintAlerts
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

    towerLockings :: Picture
    towerLockings = mconcat $ map towerLocking $ Map.assocs (_builtTowers world)

    towerLocking :: (Position, UIObject) -> Picture
    towerLocking (pos, UITower (Tower _ pic range cost (TowerLocked moRef))) =
      case PM.lookup moRef (_monsters world) of
        Just mo ->
          Color red $ Line [scalePos pos & both %~ fromIntegral, fst (_currVec mo) & both %~ fromIntegral]
        Nothing -> mempty -- This is temporary second until tower finds a new target.
    towerLocking _ = mempty

    towerRanges :: Picture
    towerRanges = _builtTowers >>> Map.assocs >>> map towerRange >>> mconcat $ world

    towerRange :: (Position, UIObject) -> Picture
    towerRange (pos, UITower (Tower _ _ range _ _)) =
      translateImg (scalePos pos, Color yellow $ Circle (fromIntegral range))

    paintAlerts :: Picture
    paintAlerts = Translate 200 (-300) $ paintAlerts' (_alerts world)

    paintAlerts' :: Q.Queue T.Text -> Picture
    paintAlerts' (Q.viewl -> Q.EmptyL) = mempty
    paintAlerts' (Q.viewl -> txt Q.:< queue) = smallText (show txt) <> Translate 0 30 (paintAlerts' queue)
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
  monsters <~ (PM.mapM moveMonster =<< use monsters)
  projectiles <~ (PM.mapM moveProjectile =<< use projectiles)
  towerShootings
  globalTime += 1
  consumeSchedEvents

  where
    moveMonster :: Maybe Monster -> SW (Maybe Monster)
    moveMonster Nothing = return Nothing
    moveMonster (Just (Monster pic speed vec f tH h)) =
      f speed vec >>= \case
        Nothing     -> return Nothing
        Just newVec -> return $ Just $ Monster pic speed newVec f tH h

    moveProjectile :: Maybe Projectile -> SW (Maybe Projectile)
    moveProjectile Nothing = return Nothing
    moveProjectile (Just (Projectile pic speed damage pos f)) = do
      time <- use globalTime
      mos <- use monsters
      f (time, mos) speed damage pos >>= \case
        Nothing     -> return Nothing
        Just newPos -> return $ Just $ Projectile pic speed damage newPos f

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
    towerShooting (pos, tower@(UITower (Tower dmg pic range cost lockState))) = do
      monsters' <- use monsters
      case lockState of
        TowerNonLocked ->
          findClosestMonster (scalePos pos) range >>= \case
            Nothing    -> return (pos, tower) -- Tower non locked and couldn't find a target
            Just moRef -> do
              -- Register shoot event here
              startShooting (scalePos pos) range moRef
              return (pos, UITower (Tower dmg pic range cost (TowerLocked moRef)))
        TowerLocked moRef ->
          case PM.lookup moRef monsters' of
            Nothing -> return (pos, UITower (Tower dmg pic range cost TowerNonLocked)) -- Target is lost
            Just monster ->
              inRange (scalePos pos) range moRef >>= \case
                True -> return (pos, tower) -- Target is alive and is in range
                False -> return (pos, UITower (Tower dmg pic range cost TowerNonLocked)) -- Target is alive but gone out of range

addEvent :: Int -> SW () -> SW ()
addEvent time' ev = do
  globalTime' <- use globalTime
  schedEvents <>= Heap.singleton (Heap.Entry (globalTime' + time') ev)

findClosestMonster :: Position -> Int -> SW (Maybe (PM.PMRef Monster))
findClosestMonster pos range = do
  monsters' <- use monsters
  return $ monsters' & findClosest
    where
      findClosest :: PM.Map Monster -> Maybe (PM.PMRef Monster)
      findClosest = PM.assocsJust
        >>> map (second (fst . _currVec)) -- [(ref, pos)]
        >>> sortBy (\(_, p1) (_, p2) -> compare (distance pos p1) (distance pos p2)) -- Sorted [(ref, pos)]
        >>> filter (\(_, pos') -> distance pos pos' < range)
        >>> headMay -- Maybe (ref, pos)
        >=> Just . fst -- Maybe ref

getRandom :: SW Int
getRandom = do
  gen <- use randGen
  let (val, newGen) = next gen
  randGen .= newGen
  return val

targetExists :: PM.PMRef Monster -> SW Bool
targetExists moRef =
  isJust . PM.lookup moRef <$> use monsters

inRange :: Position -> Int -> PM.PMRef Monster -> SW Bool
inRange pos range moRef =
  PM.lookup moRef <$> use monsters >>= \case
    Nothing -> return False
    Just monster -> return $ distance pos (fst (_currVec monster)) < range

startShooting :: Position -> Int -> PM.PMRef Monster -> SW ()
startShooting pos range target = do
  globalTime' <- gets _globalTime
  fireballPic <- use (assets . moAssets . fireball)
  let fireballObj = Projectile fireballPic 150 12 pos (projectile target)
      event = inRange pos range target >>= \case
        True -> do
          projectiles %|>>= Just fireballObj
          addEvent 1000 event
        False -> return () -- If target doesn't exist, don't schedule more shooting events
  addEvent 500 event

updateIO :: Float -> World -> IO World
updateIO _ world = runStdoutLoggingT $ flip execStateT world $ runSW update

--------------------------------------------------------------------------------

speedSync :: Int -> Int -> Bool
speedSync time speed = time `mod` (gameFreq `div` speed) == 0

projectile :: PM.PMRef Monster -> ProjectileIterator
projectile moRef (time, monsters) speed damage pos@(x, y) = do
  rand <- getRandom
  if speedSync time speed then
    case PM.lookup moRef monsters of
      Nothing -> return Nothing -- When target is lost, this projectile should also disappear
      Just target ->
        let (targetPos@(targetX, targetY), _dir) = _currVec target in
        if distance pos targetPos < 5
          then
            Nothing <$ inflictDamage moRef damage
          else
            return $ Just (moveTowards rand pos targetPos)
  else
    return $ Just pos

inflictDamage :: PM.PMRef Monster -> Damage -> SW ()
inflictDamage moRef damage = do
  mos <- use monsters
  monsters <~ PM.mapMWithKey (\ref' mbmon -> if ref' == moRef then inflictDamage' mbmon else return mbmon) mos
    where
      inflictDamage' :: Maybe Monster -> SW (Maybe Monster)
      inflictDamage' Nothing = return Nothing
      inflictDamage' (Just monster)
        | monster ^. health < damage = return Nothing -- Monster dies
        | otherwise = do
          let newMonster = monster & health -~ damage
          -- logInfo $ "Inflicted " <> T.pack (show damage) <> " damage to monster: " <> T.pack (show newMonster)
          return $ Just newMonster

gameOver :: SW ()
gameOver = error "Not implemented: gameOver"

reduceLife :: SW ()
reduceLife = do
  lives' <- playerInfo . lives <-= 1
  when (lives' <= 0) gameOver

tileFollower :: Picture -> MonsterIterator
tileFollower tile speed ((x, y), dir) = do
  tileMap' <- use wTileMap
  time <- use globalTime
  if speedSync time speed then
    if x `mod` tileSize == 0 && y `mod` tileSize == 0 then
      case availablePositions tileMap' of
        []    -> Nothing <$ reduceLife
        (x:_) -> return $ Just x
    else
      return $ Just (tupleSum (x, y) dir, dir)
  else
    return $ Just ((x, y), dir)
  where
    availablePositions tileMap' =
        -- Accept only positions that are matching to the given tile type from points of interest
        filter (\(pos', _) ->
          case Map.lookup (unscalePos pos') tileMap' of
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

buyTower :: Tower -> SW Bool
buyTower tower = do
  gold' <- use $ playerInfo . gold
  let cost = tower ^. towerCost
  if gold' >= cost
    then True <$ (playerInfo . gold -= cost)
    else return False

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
          Just (Floor True tile) ->
            buyTower tower >>= \case
              True -> do
                builtTowers %= Map.insert (adjustPosToIndex pos) (UITower tower)
                wTileMap %= Map.insert (adjustPosToIndex pos) (Floor False tile)
                selectorState .= MouseFree
              False -> do
                alerts' <- use alerts
                addAlert "Not enough gold!"
          _ -> return ()
  EventKey (MouseButton RightButton) Down _modifiers _pos ->
    selectorState .= MouseFree
  _ -> return ()

instance Show a => Show (Q.Queue a) where
  show (Q.viewl -> Q.EmptyL) = "_"
  show (Q.viewl -> val Q.:< rem) = show val <> " -> " <> show rem

eventHandlerIO :: Event -> World -> IO World
eventHandlerIO ev world =
  runStdoutLoggingT $ execStateT (runSW (eventHandler ev)) world

enqueue :: Q.Queue a -> a -> Q.Queue a
enqueue = (Q.|>)

dequeue :: Q.Queue a -> Q.Queue a
dequeue (Q.viewl -> _ Q.:< rem) = rem

addAlert :: T.Text -> SW ()
addAlert txt = do
  alerts %= (`enqueue` txt)
  addEvent 2000 $ alerts %= dequeue

pushMonsters
  :: Maybe Int
    -- ^ Base delay (optional)
  -> Int
    -- ^ Amount of objs
  -> Int
    -- ^ Interval
  -> Monster
    -- ^ Type of objs
  -> SW ()
pushMonsters mbDelay amount interval obj = do
  globalTime' <- use globalTime
  let intervals = map (* interval) [0..(amount - 1)]
      delay = fromMaybe 0 mbDelay
  forM_ intervals $ \i -> addEvent (i + delay) (monsters %|>>= Just obj)

--------------------------------------------------------------------------------

getCentaur :: SW Monster
getCentaur = do
  pic <- use $ assets . moAssets . centaur
  grass <- use $ assets . grass
  return Monster
    { _moPicture   = pic
    , _currVec     = ((28, 0), (1, 0))
    , _speed       = 50
    , _vecIterator = tileFollower grass
    , _totalHealth = 120
    , _health      = 120
    }

giveGold :: Int -> SW ()
giveGold amount = playerInfo . gold += amount

registerLevelEvents :: SW ()
registerLevelEvents = do
  fireball <- use $ assets . moAssets . fireball
  grass <- use $ assets . grass
  level <- use level
  case level of
    1 -> do
      pushMonsters (Just 1000) 3 500 =<< getCentaur
      nextLevelIn 15000 -- Go to next level after 15 secs. (disabled for now)
    2 -> do
      pushMonsters (Just 500) 6 500 =<< getCentaur
      giveGold 100
      -- nextLevelIn 15000 -- Go to next level after 15 secs. (disabled for now)
    other -> error $ "Events for level is not implemented: " <> show other

nextLevelIn :: Int -> SW ()
nextLevelIn sec = do
  addAlert "Next level!"
  addEvent sec $ do
    level += 1
    registerLevelEvents

nextLevel :: SW ()
nextLevel = do
  level += 1
  registerLevelEvents

initWorld :: Assets -> StdGen -> World
initWorld assets randGen = World
  { _level         = 0
  , _levelPic      = Blank
  , _monsters      = PM.empty
  , _wTileMap      = Map.empty
  , _globalTime    = 0
  , _schedEvents   = Heap.empty
  , _selectorState = MouseFree
  , _mousePos      = (0.0, 0.0)
  , _guiState      = initGUIState assets
  , _assets        = assets
  , _builtTowers   = Map.empty
  , _randGen       = randGen
  , _projectiles   = PM.empty
  , _playerInfo    = PlayerInfo 10 100
  , _alerts        = Q.empty
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
