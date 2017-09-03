{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Game.Pathfinding where

import Game.Types
import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import Control.Lens
import Safe (headMay)
import Game.Position hiding (Position)
import qualified Data.Map as Map

newtype PathM a = PathM { runPathM :: Reader TileMap a }
  deriving (Functor, Applicative, Monad, MonadReader TileMap)

type Position = (Float, Float)
type Vector = (Float, Float) -- like (0.67, -0.33)

-- Path nodes decider, which makes use of higher level functions below
decide
  :: AbsolutePosition
  -- Start
  -> AbsolutePosition
  -- Target
  -> PathM (Maybe [AbsolutePosition])
  -- List of positions to stop by (like stations for a train) *including* target position
decide start target = findObsticle start target >>= \case
  Nothing -> return (Just [target]) -- There are no obsticles, go to target directly
  Just obs ->
    tryResolve start obs >>= \case
      Nothing -> return Nothing -- There is no way to pass this obsticle
      Just mergeP ->
        liftA2 (++?) (decide start mergeP) (decide mergeP target)

-- * High level helper functions

(++?) :: Maybe [a] -> Maybe [a] -> Maybe [a]
Nothing ++? _ = Nothing
_ ++? Nothing = Nothing
(Just xs) ++? (Just xs') = Just (xs ++ xs')

tryResolve
  :: AbsolutePosition
  -- Start (this is necessary for using as a reference to perpendicular pair
  -> AbsolutePosition
  -- This is the obsticle. We'll start moving from here.
  -> PathM (Maybe AbsolutePosition)
  -- Nothing if we cannot find a solution. This means direct rejection of the whole path.
tryResolve (AbsolutePosition start) (AbsolutePosition obs) =
  takeWhenM pred [0..]
  where
    vec      = vectorize start obs
    (p1, p2) = perpVectors vec
    pred i = do
      r1 <- nodeAvailable (AbsolutePosition (p1 +.. (vec & both *~ i))) -- Positive prependicular vec
      r2 <- nodeAvailable (AbsolutePosition (p2 +.. (vec & both *~ i))) -- Negative prependicular vec
      if r1 then
        return (Just $ AbsolutePosition p1)
      else if r2 then
        return (Just $ AbsolutePosition p2)
      else
        return Nothing

-- | Unused for now
infBoth :: [Int]
infBoth =
  map fun [0 .. ]
    where
      fun n
        | even n = n `quot` 2
        | otherwise = (1 - n) `quot` 2

takeWhenM :: Monad m => (a -> m (Maybe k)) -> [a] -> m (Maybe k)
takeWhenM f [] = return Nothing
takeWhenM f (x:xs) = do
  result <- f x
  case result of
    Just k -> return (Just k)
    Nothing -> takeWhenM f xs

-- Perpendicular pairs of the vector (with +π/2 and -π/2 turned states)
perpVectors :: Vector -> (Vector, Vector)
perpVectors (x, y) = ((-y, x), (y, -x))

vectorize :: Position -> Position -> Vector
vectorize (x1, y1) (x2, y2) =
  let diff@(dX, dY) = (x2 - x1, y2 - y1) in
  diff & both //~ (dX + dY)

findObsticle :: AbsolutePosition -> AbsolutePosition -> PathM (Maybe AbsolutePosition)
findObsticle sa@(AbsolutePosition start@(x1, y1)) ta@(AbsolutePosition target@(x2, y2)) = do
  tileMap' <- ask
  let dirVec = vectorize start target
      testNodeVec = dirVec & both *~ fromIntegral tileSize
      testNodes = between start target testNodeVec
  headMay <$> filterM (fmap not . nodeAvailable) testNodes

(+..) :: (Float, Float) -> (Float, Float) -> (Float, Float)
a +.. (x, y) = a & (+x) *** (+y)

(<..) :: (Float, Float) -> (Float, Float) -> Bool
(x1, y1) <.. (x2, y2) = x1 < x2 && y1 < y2

-- | Move a position according to a vector
move :: Position -> Vector -> Position
move = (+..)

between
  :: Position
  -- ^ Start
  -> Position
  -- ^ End
  -> Vector
-- ^ Vector for progress (eg. (18.1, -9.0))
  -> [AbsolutePosition]
-- ^ Resulting intermediate nodes
between start end vec =
  let result = start +.. vec in
  if result <.. end
     then AbsolutePosition result : between result end vec
     else []

nodeAvailable :: AbsolutePosition -> PathM Bool
nodeAvailable pos = do
  tileMap' <- ask
  return $ case Map.lookup (convertPos pos) tileMap' of
    Just (UITower _) -> False -- There is a tower
    Just (Floor True _) -> False -- Floor that is placable on. Probably stone.
    Just (Floor False _) -> True -- Non-placable floor. Probably grass
    Nothing -> False -- There is no object at all. It's void.
