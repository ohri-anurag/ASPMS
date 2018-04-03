{-# LANGUAGE OverloadedStrings #-}
module Types(
    AccountMode(ADD,EDIT)
) where

import SP6.Data.Account
import SP6.Data.ID
-- import SP6.Data.Command

import qualified Data.HashMap.Strict as HM
import qualified Data.Array.Unboxed as A

import Data.Aeson
import Data.Hashable

-- Account Mode
data AccountMode = ADD | EDIT
    deriving Eq

instance FromJSON AreaOfControl
instance FromJSON LineOverviewConfig

instance Hashable OC_ID
instance FromJSONKey OC_ID
instance FromJSONKey StopPointCode
instance FromJSON EventTag
instance FromJSONKey EventTag

instance FromJSON Account where
    parseJSON = withObject "Account" $ \o -> do
        pwd <- o .: "accountPassword"
        name <- o .: "accountName"
        acr <- o .: "accountACR"
        aoc <- o .: "accountAOC"
        pure $ Account pwd name (A.array (OC801, OC808) $ HM.toList acr) aoc

instance FromJSON RunningTimeLists where
    parseJSON = withObject "Running Time Lists" $ \o -> do
        maximumPerformance <- o .: "maximumPerformance"
        fivePercentCoasting <- o .: "fivePercentCoasting"
        eightPercentCoasting <- o .: "eightPercentCoasting"
        energySaving <- o .: "energySaving"
        fullCoasting <- o .: "fullCoasting"
        pure $ RunningTimeLists maximumPerformance fivePercentCoasting eightPercentCoasting energySaving fullCoasting

instance FromJSON DwellTimeSets where
    parseJSON = withObject "Dwell Time Sets" $ \o -> do
        dwellTimeSet1 <- o .: "dwellTimeSet1"
        dwellTimeSet2 <- o .: "dwellTimeSet2"
        dwellTimeSet3 <- o .: "dwellTimeSet3"
        pure $ DwellTimeSets dwellTimeSet1 dwellTimeSet2 dwellTimeSet3
