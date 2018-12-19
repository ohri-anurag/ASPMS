{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Types(
    AccountMode(ADD,EDIT),
    User(ChiefController, RollingStockController, CrewController)
) where

import SP6.Data.Account
import SP6.Data.ID hiding (CrewController, RollingStockController)
import SP6.Data.Command(EventTag)

import qualified Data.HashMap.Strict as HM
import qualified Data.Array.Unboxed as A
import qualified Data.Text as T

import Data.Aeson
import Data.Hashable

import GHC.Generics (Generic)

-- Account Mode
data AccountMode = ADD | EDIT
    deriving Eq

instance FromJSON AreaOfControl
instance FromJSON LineOverviewConfig

instance Hashable OC_ID
instance FromJSONKey OC_ID
instance FromJSONKey StopPointCode
instance FromJSONKey EventTag
instance FromJSONKey RakeID
instance FromJSONKey CrewID

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

instance ToJSON AccountAndSystemParameterConfig
instance ToJSON SystemParameter
instance ToJSON Account
instance ToJSON RunningTimeLists
instance ToJSON DwellTimeSets
instance ToJSON AreaOfControl
instance ToJSON LineOverviewConfig
instance (Show a, ToJSON b, A.IArray A.UArray b, A.Ix a) => ToJSON (A.UArray a b) where
    toJSON = object . map (\(a,b) -> (T.pack $ show a, toJSON b)) . A.assocs

instance ToJSONKey UserID2
instance ToJSONKey EventTag
instance ToJSONKey StopPointCode
instance ToJSONKey RakeID
instance ToJSONKey CrewID

data User = ChiefController | RollingStockController | CrewController

instance Show User where
    show ChiefController        = "Chief Controller"
    show RollingStockController = "Rolling Stock Controller"
    show CrewController         = "Crew Controller"
