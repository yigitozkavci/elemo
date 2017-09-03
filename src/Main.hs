{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

--------------------------------------------------------------------------------
import           Control.Applicative.Extra
import           Control.Arrow.Extra
import           Control.Lens                     hiding (inside)
import           Control.Lens.Operators
import           Control.Monad
import           Control.Monad.Logger             (runStdoutLoggingT)
import           Control.Monad.Logger.CallStack   (logInfo)
import           Control.Monad.State              (execStateT, gets, modify)
import           Data.Foldable                    (toList)
import qualified Data.Heap                        as Heap
import           Data.List                        (sortBy)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust, mapMaybe)
import           Data.Monoid                      (mempty, (<>))
import qualified Data.Sequence                    as Seq
import qualified Data.Sequence.Queue              as Q
import qualified Data.Text                        as T
import           Data.Tuple                       (swap)
import qualified Data.Vector                      as V
import qualified Data.Yaml                        as Y
import           Graphics.Gloss                   hiding (blank, display)
import           Graphics.Gloss.Interface.IO.Game
import           Safe                             (headMay)
--------------------------------------------------------------------------------
import qualified Data.PreservedMap                as PM
import           Game.Assets
import           Game.Position
import           Game.Tilegen                     hiding (_tileMap)
import           Game.Types
import           Game.Utils
import           Game.Impure                      (readTowers)
import           Game.Direction
--------------------------------------------------------------------------------

adjustPosToIndex :: (Float, Float) -> TilePosition
adjustPosToIndex = TilePosition .
  over both (
        (+ (fromIntegral tileSize / 2))
    >>> (/ fromIntegral tileSize)
    >>> floor
  )

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
             <> Translate 200 (-60) (smallText "ALERTS")
             <> Translate 200 (-70) (smallText "======")
             <> Translate 200 (-100) paintAlerts
             <> Translate (-200) (-60) (smallText "EVENTS")
             <> Translate (-200) (-70) (smallText "======")
             <> Translate (-200) (-100) eventDebugScreen
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

    lineAbs :: [AbsolutePosition] -> Picture
    lineAbs xs = Line $ map (\(AbsolutePosition pos) -> pos) xs

    towerLocking :: (TilePosition, UIObject) -> Picture
    towerLocking (pos, UITower (Tower _ pic range cost (TowerLocked moRef) _)) =
      case PM.lookup moRef (_monsters world) of
        Just mo ->
          Color red $ lineAbs [convertPos pos, fst (_currVec mo)]
        Nothing -> mempty -- This is temporary second until tower finds a new target.
    towerLocking _ = mempty

    towerRanges :: Picture
    towerRanges = _builtTowers >>> Map.assocs >>> map towerRange >>> mconcat $ world

    towerRange :: (TilePosition, UIObject) -> Picture
    towerRange (pos, UITower (Tower _ _ range _ _ _)) =
      translateImg (convertPos pos, Color yellow $ Circle (fromIntegral range))

    paintAlerts :: Picture
    paintAlerts = paintAlerts' (_alerts world)

    paintAlerts' :: Q.Queue T.Text -> Picture
    paintAlerts' (Q.viewl -> Q.EmptyL) = mempty
    paintAlerts' (Q.viewl -> txt Q.:< queue) = smallText (show txt) <> Translate 0 (-30) (paintAlerts' queue)

    eventDebugScreen :: Picture
    eventDebugScreen =
          Heap.toUnsortedList
      >>> map (\(Heap.Entry k v) -> (k, v))
      >>> map (second (T.unpack . fst))
      >>> map (first show)
      >>> smallTexts
      $ _schedEvents world
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

updateProjectile :: PM.PMRef Projectile -> (Projectile -> Projectile) -> SW ()
updateProjectile ref f =
  projectiles %= PM.update (\case Nothing -> Nothing; Just p -> Just (f p)) ref

updateMonster :: PM.PMRef Monster -> (Monster -> Monster) -> SW ()
updateMonster moRef f =
  monsters %= PM.update (\case Nothing -> Nothing; Just monster -> Just (f monster)) moRef

update :: SW ()
update = do
  tilegenLevel'
  tilegenGUI'
  use builtTowers >>= (wTileMap <>=)
  consumeSchedEvents
  use monsters <&> PM.assocs >>= mapM_ moveMonster
  use projectiles <&> PM.assocs >>= mapM_ moveProjectile
  use builtTowers <&> Map.assocs >>= mapM_ handleTowerShooting
  globalTime += 1

  where
    moveMonster :: (PM.PMRef Monster, Maybe Monster) -> SW ()
    moveMonster (_, Nothing) = return ()
    moveMonster (moRef, Just monster@(Monster _ speed vec f _ _ _)) =
      f speed vec >>= \case
        Nothing -> monsters %= PM.delete moRef
        Just newVec -> updateMonster moRef (currVec .~ newVec)

    moveProjectile :: (PM.PMRef Projectile, Maybe Projectile) -> SW ()
    moveProjectile (_, Nothing) = return ()
    moveProjectile (ref, Just (Projectile pic speed damage pos f)) = do
      time <- use globalTime
      mos <- use monsters
      f (time, mos) speed damage pos >>= \case
        Nothing -> projectiles %= PM.delete ref
        Just position -> updateProjectile ref (projectilePosition .~ position)

    consumeSchedEvents :: SW ()
    consumeSchedEvents = do
      globalTime' <- use globalTime
      ev <- Heap.viewMin <$> use schedEvents
      forM_ ev $ \(Heap.Entry time (_, f), newHeap) ->
        when (time == globalTime') $ do
          schedEvents .= newHeap
          f -- Actual event mutation
          consumeSchedEvents

    handleTowerShooting :: (TilePosition, UIObject) -> SW ()
    handleTowerShooting (pos, UITower tower@(Tower _dmg _pic range _cost lockState' canShoot')) = do
      monsters' <- use monsters
      case lockState' of
        TowerNonLocked ->
          findClosestMonster (convertPos pos) range >>= \case
            Nothing    -> return () -- Tower non locked and couldn't find a target
            Just moRef ->
              lockTower pos moRef
        TowerLocked moRef ->
          case PM.lookup moRef monsters' of
            Nothing ->
              unlockTower pos
            Just monster -> do
              inRange' <- inRange (convertPos pos) range moRef
              if inRange' then
                when canShoot' $ shoot (pos, tower) range moRef
              else
                unlockTower pos

lockTower :: TilePosition -> PM.PMRef Monster -> SW ()
lockTower pos moRef =
  updateTower pos (lockState .~ TowerLocked moRef)

unlockTower :: TilePosition -> SW ()
unlockTower pos =
  updateTower pos (lockState .~ TowerNonLocked)

updateTower :: TilePosition -> (Tower -> Tower) -> SW ()
updateTower pos f =
  builtTowers %= Map.update (\(UITower t) -> Just . UITower $ f t) pos

addEvent :: Int -> T.Text -> SW () -> SW ()
addEvent time' description ev = do
  globalTime' <- use globalTime
  schedEvents <>= Heap.singleton (Heap.Entry (globalTime' + time') (description, ev))

findClosestMonster :: AbsolutePosition -> Int -> SW (Maybe (PM.PMRef Monster))
findClosestMonster pos range =
  use monsters <&> f
    where
      f :: PM.Map Monster -> Maybe (PM.PMRef Monster)
      f =   PM.assocsJust
        >>> map (second (fst . _currVec)) -- [(ref, pos)]
        >>> sortBy (\(_, p1) (_, p2) -> compare (distance pos p1) (distance pos p2)) -- Sorted [(ref, pos)]
        >>> filter (\(_, pos') -> distance pos pos' < range)
        >>> headMay -- Maybe (ref, pos)
        >=> Just . fst -- I wish precedence of >>> and <&> wouldn't be equal...

targetExists :: PM.PMRef Monster -> SW Bool
targetExists moRef =
  isJust . PM.lookup moRef <$> use monsters

inRange :: AbsolutePosition -> Int -> PM.PMRef Monster -> SW Bool
inRange pos range moRef =
  use monsters <&> PM.lookup moRef >>= \case
    Nothing -> return False
    Just monster -> return $ distance pos (fst (_currVec monster)) < range

shoot :: (TilePosition, Tower) -> Int -> PM.PMRef Monster -> SW ()
shoot (pos, tower) range target = do
  globalTime' <- use globalTime
  fireballPic <- use (assets . moAssets . fireball)
  let fireballObj = Projectile fireballPic 150 (_damage tower) (convertPos pos) (projectile target)
  projectiles %|>>= Just fireballObj
  updateTower pos (canShoot .~ False) -- After this function is called, we restrict shooting of this tower no matter what.
  addEvent 2000 "Allow shooting" $ updateTower pos (canShoot .~ True)

updateIO :: Float -> World -> IO World
updateIO _ world = runStdoutLoggingT $ flip execStateT world $ runSW update

--------------------------------------------------------------------------------

speedSync :: Int -> Int -> Bool
speedSync time speed = time `mod` (gameFreq `div` speed) == 0

projectile :: PM.PMRef Monster -> ProjectileIterator
projectile moRef (time, monsters) speed damage pos =
  if speedSync time speed then
    case PM.lookup moRef monsters of
      Nothing -> return Nothing -- When target is lost, this projectile should also disappear
      Just target ->
        let (targetPos, _dir) = _currVec target in
        if distance pos targetPos < 5
          then
            Nothing <$ inflictDamage moRef damage
          else
            return $ Just (moveTowards pos targetPos)
  else
    return $ Just pos

inflictDamage :: PM.PMRef Monster -> Damage -> SW ()
inflictDamage moRef damage = do
  -- Replace with monster ref map mapping
  mos <- use monsters
  monsters <~ PM.mapMWithKey (\ref' mbmon -> if ref' == moRef then inflictDamage' mbmon else return mbmon) mos
    where
      inflictDamage' :: Maybe Monster -> SW (Maybe Monster)
      inflictDamage' Nothing = return Nothing
      inflictDamage' (Just monster)
        | monster ^. health < damage = Nothing <$ giveGold (monster ^. goldYield) -- Monster dies
        | otherwise = do
          let newMonster = monster & health -~ damage
          return $ Just newMonster

gameOver :: SW ()
gameOver = error "Not implemented: gameOver"

reduceLife :: SW ()
reduceLife = do
  lives' <- playerInfo . lives <-= 1
  when (lives' <= 0) gameOver

tileFollower :: Picture -> MonsterIterator
tileFollower tile speed (pos, dir) = do
  tileMap' <- use wTileMap
  time <- use globalTime
  if speedSync time speed then
    if matchesTile pos then
      case availablePositions tileMap' of
        []    -> Nothing <$ reduceLife
        (x:_) -> return $ Just x
    else
      let TilePosition dir' = serializeDir dir
          dir'' = AbsolutePosition (dir' & both %~ fromIntegral)
      in
          return $ Just (pos +. dir'', dir)
  else
    return $ Just (pos, dir)
  where
    availablePositions tileMap' =
        -- Accept only positions that are matching to the given tile type from points of interest
        filter (\(result, test, dir) ->
          case Map.lookup test tileMap' of
            Just obj
              | getPicture obj == tile -> True
            _ -> False
        )
        -- After filtering, direction addings must be cut. We don't want `tileSize` amount
        -- of movement, afterall.
        >>> map (\(result, _test, dir) -> (result, dir))
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
    -- For arrow computation;
    -- ('result of moving', 'tile position to check', 'original direction')
    posOfIntr :: [(AbsolutePosition, TilePosition, Direction)]
    posOfIntr =
      map ((serializeDir >>> flip moveWithDir pos) &&& (serializeDir >>> (convertPos pos +.)) &&&. id)
        (arrowDir dir)

moveWithDir :: TilePosition -> AbsolutePosition -> AbsolutePosition
moveWithDir (TilePosition dir) = (+.) $ AbsolutePosition (dir & both %~ fromIntegral)

--------------------------------------------------------------------------------

guiClick :: (Float, Float) -> SW (Maybe UIObject)
guiClick pos = use wTileMap <&> Map.lookup (adjustPosToIndex pos)

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
              False -> do
                alerts' <- use alerts
                addAlert "Not enough gold!"
          _ -> return ()
  EventKey (MouseButton RightButton) Down _modifiers _pos ->
    selectorState .= MouseFree
  _ -> return ()

instance Show a => Show (Q.Queue a) where
  show (Q.viewl -> Q.EmptyL)     = "_"
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
  addEvent 2000 "Remove alert" $ alerts %= dequeue

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
  forM_ intervals $ \i -> addEvent (i + delay) "Push monster" (monsters %|>>= Just obj)

--------------------------------------------------------------------------------

getCentaur :: SW Monster
getCentaur = do
  pic <- use $ assets . moAssets . centaur
  grass <- use $ assets . grass
  return Monster
    { _moPicture   = pic
    , _currVec     = (TilePosition (1, 0) & convertPos, DRight)
    , _speed       = 50
    , _vecIterator = tileFollower grass
    , _totalHealth = 120
    , _health      = 120
    , _goldYield   = 10
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
      pushMonsters (Just 1000) 1 500 =<< getCentaur
      giveGold 70
      nextLevelIn 7000
    2 -> do
      pushMonsters (Just 500) 2 500 =<< getCentaur
      giveGold 70
      nextLevelIn 12000 -- Go to next level after 15 secs. (disabled for now)
    3 -> do
      pushMonsters (Just 500) 4 500 =<< getCentaur
      giveGold 100
      -- nextLevelIn 5000 -- Go to next level after 15 secs. (disabled for now)
    other -> error $ "Events for level is not implemented: " <> show other

nextLevelIn :: Int -> SW ()
nextLevelIn sec = do
  addAlert "Next level!"
  addEvent sec "Next level" $ do
    level += 1
    registerLevelEvents

nextLevel :: SW ()
nextLevel = do
  level += 1
  registerLevelEvents

initWorld :: Assets -> Seq.Seq Tower -> World
initWorld assets towers = World
  { _level         = 0
  , _levelPic      = Blank
  , _monsters      = PM.empty
  , _wTileMap      = Map.empty
  , _globalTime    = 0
  , _schedEvents   = Heap.empty
  , _selectorState = MouseFree
  , _mousePos      = (0.0, 0.0)
  , _guiState      = (initGUIState assets) { _guiTowers = towers }
  , _assets        = assets
  , _builtTowers   = Map.empty
  , _projectiles   = PM.empty
  , _playerInfo    = PlayerInfo 10 0
  , _alerts        = Q.empty
  }

gameFreq :: Int
gameFreq = 1000

main :: IO ()
main = do
  assets <- genAssets
  towers <- readTowers
  initWorld' <- runStdoutLoggingT $ flip execStateT (initWorld assets towers) $ runSW nextLevel
  playIO
    (InWindow "Nice Window" (700, 700) (0, 0))
    white
    gameFreq
    initWorld'
    displayIO
    eventHandlerIO
    updateIO
