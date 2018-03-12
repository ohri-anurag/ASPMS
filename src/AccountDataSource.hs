{-# LANGUAGE OverloadedStrings #-}
module AccountDataSource (
    getData,
    putData,
    updateAccount,
    updateSystemParams,
    updateRunningTimeLists,
    AccountAndSystemParameterConfig
) where

-- Data Type Definitions
import SP6.Data.Account
import SP6.Data.ID
import SP6.Data.Command

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Array.Unboxed as A

import Text.Read(readMaybe)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.ByteString as B
import qualified Data.Serialize as S
import Data.Aeson
import Data.Hashable

instance FromJSON AreaOfControl
instance FromJSON LineOverviewConfig

instance Hashable OC_ID
instance FromJSONKey OC_ID

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

getData :: IO AccountAndSystemParameterConfig
getData = (f . S.decode) <$> B.readFile "data/AccountData"
    where
        f x = case x of
            Left _ -> error "Could not read account data."
            Right val -> val

putData :: AccountAndSystemParameterConfig -> IO ()
putData accountData = B.writeFile "data/AccountData" (S.encode accountData)

-- Assumption that JSON data is being sent to the server.
createAccount :: [(T.Text, T.Text)] -> Maybe (UserID, Account)
createAccount paramList = do
    text <- lookup "userID" paramList
    uid <- readMaybe $ T.unpack text
    json <- lookup "data" paramList
    acc <- decodeStrict' $ TE.encodeUtf8 json
    Just (uid, acc)

    -- FOR DEBUGGING
    -- case lookup "userID" paramList of
    --     Nothing -> error "No userID found"
    --     Just text -> case readMaybe $ T.unpack text of
    --         Nothing -> error "Invalid userID"
    --         Just uid -> case lookup "data" paramList of
    --             Nothing -> error "No JSON found"
    --             Just json -> case decodeStrict' $ TE.encodeUtf8 json of
    --                 Nothing -> error "Invalid JSON"
    --                 Just acc -> Just (uid, acc)

modifyAccount :: (UserID, Account) -> AccountAndSystemParameterConfig -> AccountAndSystemParameterConfig
modifyAccount (uid, acc) (AccountAndSystemParameterConfig accConf sysParam) = AccountAndSystemParameterConfig (M.insert uid acc accConf) sysParam

updateAccount :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateAccount ps accConfSysParam = createAccount ps >>= Just . flip modifyAccount accConfSysParam

updateSystemParams :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateSystemParams ps accConfSysParam = do
    departureOffsetStr <- lookup "departureOffset" ps
    departureOffset <- readMaybe $ T.unpack departureOffsetStr
    routeTriggerOffsetStr <- lookup "routeTriggerOffset" ps
    routeTriggerOffset <- readMaybe $ T.unpack routeTriggerOffsetStr
    minimumDwellTimeStr <- lookup "minimumDwellTime" ps
    minimumDwellTime <- readMaybe $ T.unpack minimumDwellTimeStr
    delayDetectionThreshHoldStr <- lookup "delayDetectionThreshHold" ps
    delayDetectionThreshHold <- readMaybe $ T.unpack delayDetectionThreshHoldStr
    intestationStopDetectionTimeStr <- lookup "interstationStopDetectionTime" ps
    intestationStopDetectionTime <- readMaybe $ T.unpack intestationStopDetectionTimeStr
    tunnelLimitStr <- lookup "tunnelLimit" ps
    tunnelLimit <- readMaybe $ T.unpack tunnelLimitStr
    Just $ accConfSysParam {
        systemParameter = (systemParameter accConfSysParam) {
            departureOffset = departureOffset,
            routeTriggerOffset = routeTriggerOffset,
            minimumDwellTime = minimumDwellTime,
            delayDetectionThreshHold = delayDetectionThreshHold,
            intestationStopDetectionTime = intestationStopDetectionTime,
            tunnelLimit = tunnelLimit
        }
    }

updateRunningTimeLists :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateRunningTimeLists ps accConfSysParam = do
    json <- lookup "data" ps
    runningTimeList <- decodeStrict' $ TE.encodeUtf8 json
    Just $ accConfSysParam {
        systemParameter = (systemParameter accConfSysParam) {
            runningTimeList = runningTimeList
        }
    }

    -- FOR DEBUGGING
    -- case createAccount ps of
    --     Nothing -> error "Could not create account"
    --     Just x -> Just (modifyAccount x accConf)

-- FAKE DATA

-- accountAndSystemParameterConfig :: AccountAndSystemParameterConfig
-- accountAndSystemParameterConfig = AccountAndSystemParameterConfig {
--         accountConfig = fakeAccountConfig,
--         systemParameter = fakeSystemParameter
--     }
--
-- fakeAccountConfig :: M.Map UserID Account
-- fakeAccountConfig = M.fromList [
--         (TTM_OFF, Account { accountPassword = "pass1", accountName = "name1", accountACR = fakeAccountACR, accountAOC = fakeAccountAOC }),
--         (TTM_OFF_BCC, Account { accountPassword = "pass2", accountName = "name2", accountACR = fakeAccountACR, accountAOC = fakeAccountAOC })
--     ]
--
-- fakeAccountACR :: A.UArray OC_ID Bool
-- fakeAccountACR = A.array (OC801, OC808) $ zip [OC801 ..] $ repeat False
--
-- fakeAccountAOC = AreaOfControl {
--         aocLineOverview           = Just fakeLineOverviewConfig
--         , aocMaintenanceMonitor     = False
--         , aocTimetableManagement    = False
--         , aocRollingStockController = False
--         , aocCrewController         = False
--         , aocRollingStockManagement = False
--     }
--
-- fakeLineOverviewConfig = LineOverviewConfig{
--         enableGlobalCommand     = False
--         , enableEnableRegulation  = False
--     }
--
-- fakeSystemParameter = SystemParameter
--     { departureOffset = 0
--     , routeTriggerOffset = 0
--     , minimumDwellTime = 0
--     , delayDetectionThreshHold = 0
--     , intestationStopDetectionTime = 0
--     , tunnelLimit = 0
--     , runningTimeList = fakeRunningTimeLists
--     , dwellTimeSet = fakeDwellTimeSets
--     , alarmLevel = fakeAlarmLevel
--     }
--
-- fakeRunningTimeLists = RunningTimeLists
--     { maximumPerformance   = fakeRunningTimeList
--     , fivePercentCoasting  = fakeRunningTimeList
--     , eightPercentCoasting = fakeRunningTimeList
--     , energySaving         = fakeRunningTimeList
--     , fullCoasting         = fakeRunningTimeList
--     }
--
-- fakeRunningTimeList = M.fromList
--     [ ((SP_C1, SP_31), 62.7)
--     , ((SP_C2, SP_31), 84.1)
--     , ((SP_31, SP_33), 104.6)
--     , ((SP_32, SP_33), 122.6)
--     , ((SP_C3, SP_33), 999)
--     , ((SP_33, SP_35), 104.9)
--     , ((SP_35, SP_37), 98.5)
--     , ((SP_37, SP_39), 177.1)
--     , ((SP_39, SP_D6), 42.3)
--     , ((SP_39, SP_41), 134.9)
--     , ((SP_40, SP_41), 146.2)
--     , ((SP_D6, SP_41), 999)
--     , ((SP_C5, SP_41), 51.6)
--     , ((SP_41, SP_43), 145.5)
--     , ((SP_41, SP_C6), 58.1)
--     , ((SP_43, SP_45), 127.8)
--     , ((SP_45, SP_47), 114.4)
--     , ((SP_47, SP_49), 91.7)
--     , ((SP_49, SP_51), 75.3)
--     , ((SP_49, SP_C7), 55.9)
--     , ((SP_49, SP_C8), 62.8)
--     , ((SP_50, SP_51), 92)
--     , ((SP_51, SP_53), 80.8)
--     , ((SP_53, SP_55), 114.3)
--     , ((SP_55, SP_57), 74)
--     , ((SP_57, SP_59), 70.7)
--     , ((SP_59, SP_61), 97.8)
--     , ((SP_61, SP_63), 65.2)
--     , ((SP_61, SP_64), 73.6)
--     , ((SP_C9, SP_63), 37.9)
--     , ((SP_C9, SP_64), 54.6)
--     , ((SP_63, SP_65), 79.4)
--     , ((SP_65, SP_67), 85.4)
--     , ((SP_65, SP_68), 95)
--     , ((SP_D1, SP_67), 43)
--     , ((SP_D1, SP_68), 47.6)
--     , ((SP_67, SP_69), 96.5)
--     , ((SP_69, SP_71), 93.6)
--     ]
--
-- fakeDwellTimeSets = DwellTimeSets
--     { dwellTimeSet1 = fakeDwellTime
--     , dwellTimeSet2 = fakeDwellTime
--     , dwellTimeSet3 = fakeDwellTime
--     }
--
-- fakeDwellTime = M.fromList
--     [ (SP_31, 30)
--     , (SP_32, 30)
--     , (SP_33, 30)
--     , (SP_34, 30)
--     , (SP_35, 30)
--     , (SP_36, 30)
--     , (SP_37, 30)
--     , (SP_38, 30)
--     , (SP_39, 30)
--     , (SP_40, 30)
--     , (SP_41, 30)
--     , (SP_42, 30)
--     , (SP_43, 30)
--     , (SP_44, 30)
--     , (SP_45, 30)
--     , (SP_46, 30)
--     , (SP_47, 30)
--     , (SP_48, 30)
--     , (SP_49, 30)
--     , (SP_50, 30)
--     , (SP_51, 30)
--     , (SP_52, 30)
--     , (SP_53, 30)
--     , (SP_54, 30)
--     , (SP_55, 30)
--     , (SP_56, 30)
--     , (SP_57, 30)
--     , (SP_58, 30)
--     , (SP_59, 30)
--     , (SP_60, 30)
--     , (SP_61, 30)
--     , (SP_62, 30)
--     , (SP_63, 30)
--     , (SP_64, 30)
--     , (SP_65, 30)
--     , (SP_66, 30)
--     , (SP_67, 30)
--     , (SP_68, 30)
--     , (SP_69, 30)
--     , (SP_70, 30)
--     , (SP_71, 30)
--     , (SP_72, 30)
--     , (SP_73, 30)
--     , (SP_74, 30)
--     , (SP_75, 30)
--     , (SP_76, 30)
--     , (SP_77, 30)
--     , (SP_78, 30)
--     , (SP_79, 30)
--     , (SP_80, 30)
--     , (SP_81, 30)
--     , (SP_C1, 30)
--     , (SP_C2, 30)
--     , (SP_C3, 30)
--     , (SP_C4, 30)
--     , (SP_C5, 30)
--     , (SP_C6, 30)
--     , (SP_C7, 30)
--     , (SP_C8, 30)
--     , (SP_C9, 30)
--     , (SP_D0, 30)
--     , (SP_D1, 30)
--     , (SP_D2, 30)
--     , (SP_D3, 30)
--     , (SP_D4, 30)
--     , (SP_D5, 30)
--     , (SP_D6, 30)
--     , (SP_D7, 30)
--     , (SP_D9, 30)
--     ]
--
-- fakeAlarmLevel = M.fromList [(EventTagUndefined, ALevel1)]
