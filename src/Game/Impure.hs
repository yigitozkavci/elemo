{-# LANGUAGE OverloadedStrings   #-}

module Game.Impure (
  readTowers
) where

import Game.Types
import qualified Data.Yaml as Y
import qualified Data.Sequence as Seq
import qualified Data.ByteString                  as BS
import           Control.Monad (forM)
import Graphics.Gloss

data RawTower = RawTower
  { _rawTowerDamage :: Int 
  , _rawTowerRange :: Int
  , _rawTowerCost :: Int
  , _rawTowerPicPath :: FilePath
  , _rawTowerPeriod :: Int
  }

instance Y.FromJSON RawTower where
  parseJSON (Y.Object v) =
    RawTower
      <$> v Y..: "damage"
      <*> v Y..: "range"
      <*> v Y..: "cost"
      <*> v Y..: "picturePath"
      <*> v Y..: "period"

fromRawTower :: RawTower -> IO Tower
fromRawTower (RawTower damage range cost picPath period) = do
  image <- loadBMP picPath
  return $ Tower damage period image range cost TowerNonLocked True

readTowers :: IO (Seq.Seq Tower)
readTowers = do
  rawTowers <- Y.decode <$> BS.readFile "towers.yaml"
  case rawTowers of
    Nothing -> error "Cannot parse towers"
    Just rawTowers -> Seq.fromList <$> forM rawTowers fromRawTower
