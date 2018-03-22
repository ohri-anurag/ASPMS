{-# LANGUAGE OverloadedStrings #-}
module AccountDataSource (
    getDataBytes,
    getData,
    putData,
    updateAccount,
    deleteAccount,
    updateSystemParams,
    updateRunningTimeLists,
    updateDwellTimeSets,
    updateAlarmLevels,
    AccountAndSystemParameterConfig
) where

-- Data Type Definitions
import SP6.Data.Account
import SP6.Data.ID
import SP6.Data.Command

import Types
import Utility

import Data.Derive.Class.Default

import qualified Data.Map.Strict as M

import Control.Exception.Base(SomeException, catch)
import Text.Read(readMaybe)
import Data.Either(either)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.ByteString as B
import qualified Data.Serialize as S
import Data.Aeson


getDataBytes :: FilePath -> IO B.ByteString
getDataBytes path = catch (B.readFile path) handler
    where
    handler :: SomeException -> IO B.ByteString
    handler e = do
        debugMain $ "Could not read data from file. Resorting to default values. Error : " ++ (show e)
        pure $ S.encode (def :: AccountAndSystemParameterConfig)

getData :: IO AccountAndSystemParameterConfig
getData = either (const def) id
    <$> S.decode
    <$> getDataBytes accountFilePath

putData :: AccountAndSystemParameterConfig -> IO ()
putData accountData = B.writeFile accountFilePath (S.encode accountData)

-- Assumption that JSON data is being sent to the server.
createAccount :: [(T.Text, T.Text)] -> Maybe (UserID2, Account)
createAccount paramList = do
    text <- lookup "userID" paramList
    json <- lookup "data" paramList
    acc <- decodeStrict' $ TE.encodeUtf8 json
    Just (UserID2 $ T.unpack text, acc)

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

modifyAccount :: (UserID2, Account) -> AccountAndSystemParameterConfig -> AccountAndSystemParameterConfig
modifyAccount (uid, acc) (AccountAndSystemParameterConfig accConf sysParam) = AccountAndSystemParameterConfig (M.insert uid acc accConf) sysParam

updateAccount :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateAccount ps accConfSysParam = flip modifyAccount accConfSysParam <$> createAccount ps

deleteAccount :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
deleteAccount ps (AccountAndSystemParameterConfig accConf sysParam) = do
    flip AccountAndSystemParameterConfig sysParam
    <$> delete
    <$> lookup "userID" ps
    where
        delete uid = M.delete (UserID2 $ T.unpack uid) accConf

updateSystemParams :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateSystemParams ps accConfSysParam = do
    departureOffset <- get "departureOffset"
    routeTriggerOffset <- get "routeTriggerOffset"
    minimumDwellTime <- get "minimumDwellTime"
    delayDetectionThreshHold <- get "delayDetectionThreshHold"
    intestationStopDetectionTime <- get "interstationStopDetectionTime"
    tunnelLimit <- get "tunnelLimit"
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
    where
        get key = lookup key ps >>= readMaybe . T.unpack

updateRunningTimeLists :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateRunningTimeLists ps accConfSysParam = do
    json <- lookup "data" ps
    runningTimeList <- decodeStrict' $ TE.encodeUtf8 json
    Just $ accConfSysParam {
        systemParameter = (systemParameter accConfSysParam) {
            runningTimeList = runningTimeList
        }
    }

updateDwellTimeSets :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateDwellTimeSets ps accConfSysParam = do
    json <- lookup "data" ps
    dwellTimeSet <- decodeStrict' $ TE.encodeUtf8 json
    Just $ accConfSysParam {
        systemParameter = (systemParameter accConfSysParam) {
            dwellTimeSet = dwellTimeSet
        }
    }

updateAlarmLevels :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateAlarmLevels ps accConfSysParam = do
    json <- lookup "data" ps
    alarmLevel <- decodeStrict' $ TE.encodeUtf8 json
    Just $ accConfSysParam {
        systemParameter = (systemParameter accConfSysParam) {
            alarmLevel = alarmLevel
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
-- fakeAccountConfig :: M.Map UserID2 Account
-- fakeAccountConfig = M.fromList [
--         (UserID2 "User 1", Account { accountPassword = "pass1", accountName = "name1", accountACR = fakeAccountACR, accountAOC = fakeAccountAOC }),
--         (UserID2 "User 2", Account { accountPassword = "pass2", accountName = "name2", accountACR = fakeAccountACR, accountAOC = fakeAccountAOC })
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
--         , enableRegulation  = False
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
-- fakeAlarmLevel = M.fromList
--     [ (TagEventTrainDelayed, ALevel1) -- Setting JourneyID (Maybe RakeID) [SC_ID]
--     , (TagEventTrainControlledByTimetable, ALevel1) -- Setting RakeID (Maybe JourneyID) [SC_ID]
--     , (TagEventNoOneControlArea, ALevel1) -- OC_ID
--     , (TagEventPSDIsolated, ALevel1) -- Setting (StationCode, PlatformNumber) PlatformDoorID
--     , (TagEventPSDBypassed, ALevel1) -- Setting (StationCode, PlatformNumber) PlatformDoorID
--     , (TagEventTrainPassengerEmergencyAlarm, ALevel1) -- RakeID CarIndex [SC_ID]
--     , (TagEventTrainFireDetected, ALevel1) -- Setting RakeID CarIndex [SC_ID]
--     -- SCStatusEvent
--         -- SCBlockEvent SCBlockEvent BlockID
--     , (TagEventNoEntry, ALevel1) -- Setting
--     , (TagEventTSR, ALevel1) -- (Maybe Int)
--     , (TagEventLowAdhesion, ALevel1) -- Setting
--         -- SCTrainEvent SCTrainEvent [SC_ID] (RakeID, Maybe Location)
--     , (TagEventTrainDoorOpened, ALevel1)
--     , (TagEventTrainDoorClosed, ALevel1)
--     , (TagEventTrainShortStopped, ALevel1)
--     , (TagEventTrainSkipStop, ALevel1) -- Setting
--     , (TagEventTrainOverrun, ALevel1)
--     , (TagEventVOBCReset, ALevel1)
--     , (TagEventTrainATODeparture, ALevel1)
--     , (TagEventTrainReadyRequestIssued, ALevel1)
--     , (TagEventTrainDocked, ALevel1)
--     , (TagEventTrainPassedP0, ALevel1)
--     , (TagEventTrainDeparted, ALevel1)
--     , (TagEventTrainStaticTestCompleted, ALevel1)     -- Word16
--     , (TagEventTrainDynamicTestCompleted, ALevel1)    -- Word16
--     , (TagEventTrainDrivingModeChanged, ALevel1)      -- (Maybe DrivingMode)
--     , (TagEventTrainSleepStatusChanged, ALevel1)      -- (Maybe SleepModeStatus)
--     , (TagEventTrainEBStatusChangedSC, ALevel1)       -- (Maybe EBReasonSC)
--     , (TagEventTrainEBStatusChangedVOBC, ALevel1)     -- (Maybe EBReasonVOBC)
--     , (TagEventTrainFrontBlockChanged, ALevel1)       -- (Either BlockID PointID)
--     , (TagEventTrainLost, ALevel1)
--     , (TagEventTrainEmerged, ALevel1)
--     , (TagEventTrainRemoved, ALevel1)
--     , (TagEventTrainEnterMainline, ALevel1)
--     , (TagEventTrainLeaveMainline, ALevel1)
--     , (TagEventTrainInterstationStopped, ALevel1)
--         -- SCStationEvent SCStationEvent StopPointCode
--     , (TagEventPlatformHold, ALevel1) -- Setting
--         -- Other
--     , (TagEventZoneHold, ALevel1) -- Setting SC_ID
--     -- OCStatusEvent
--         -- OCPlatformEvent OCPlatformEvent StopPointCode
--     , (TagEventPSDOpened, ALevel1)
--     , (TagEventPSDClosed, ALevel1)
--     , (TagEventESP, ALevel1) -- Setting
--         -- OCTrackEvent OCTrackEvent TrackID
--     , (TagEventTrackOccupied, ALevel1) -- Setting
--     , (TagEventMaintenanceBlock, ALevel1) -- Setting
--     , (TagEventTrackCETROccupied, ALevel1) -- Setting
--         -- OCSignalEvent OCSignalEvent SignalID
--     , (TagEventSignalAspectChanged, ALevel1) -- Aspect
--     , (TagEventSignalLampFailured, ALevel1)  -- Aspect
--     , (TagEventSignalBlock, ALevel1) -- Setting
--         -- OCPointEvent OCPointEvent PointID
--     , (TagEventPointControlledToNormal, ALevel1)
--     , (TagEventPointControlledToReverse, ALevel1)
--     , (TagEventPointSwitchedToNormal, ALevel1)
--     , (TagEventPointSwitchedToReverse, ALevel1)
--     , (TagEventPointBlock, ALevel1) -- Setting
--     , (TagEventPointSelfNormalizationInhibition, ALevel1) -- Setting
--     , (TagEventPointManualAuthorization, ALevel1) -- Setting
--     , (TagEventEKTKeyInsertion, ALevel1) -- Setting
--         -- OCRouteEvent OCRouteEvent RouteID
--     , (TagEventSignalFleeting, ALevel1) -- Setting
--     , (TagEventRouteControl, ALevel1)   -- Setting
--     , (TagEventRouteBlock, ALevel1)     -- Setting
--         -- Other
--     , (TagEventOverlapLocking, ALevel1) -- Setting VL_ID
--     , (TagEventCycle, ALevel1) -- Setting CycleID
--     , (TagEventEmergencyMode, ALevel1) -- Setting EMSectionID
--     , (TagEventVDUMode, ALevel1) -- Setting OC_ID
--     , (TagEventSPKAuthorized, ALevel1) -- Setting SPK_ID
--     , (TagEventSPKRemoved, ALevel1) -- Setting SPK_ID
--         -- , (TagOCEMRouteEvent, ALevel1) OCEMRouteEvent EMRouteID
--     , (TagEventEMRouteControl, ALevel1)   -- Setting
--     , (TagEventEMRouteProceed, ALevel1)   -- Setting
--     -- FailureEvent
--         -- CBIFailureEvent CBIFailureEvent
--     , (TagEventMinorFailure_OC, ALevel1)  -- ExOC_ID
--     , (TagEventFailure_AM_LAN, ALevel1)   -- ExOC_ID SystemID
--     , (TagEventFailure_Backup_L, ALevel1) -- ExOC_ID SystemID
--     , (TagEventFailure_Backup_R, ALevel1) -- ExOC_ID SystemID
--     , (TagEventFailure_5V2ALM, ALevel1)   -- ExOC_ID SystemID
--     , (TagEventFailure_5V1ALM, ALevel1)   -- ExOC_ID SystemID
--     , (TagEventFailure_24V2ALM, ALevel1)  -- ExOC_ID SystemID
--     , (TagEventFailure_24V1ALM, ALevel1)  -- ExOC_ID SystemID
--     , (TagEventFailure_ACEHFB, ALevel1)   -- ExOC_ID SystemID
--     , (TagEventFailure_API1, ALevel1)     -- ExOC_ID SystemID
--     , (TagEventFailure_API2, ALevel1)     -- ExOC_ID SystemID
--     , (TagEventFailure_API3, ALevel1)     -- ExOC_ID SystemID
--     , (TagEventFailure_APO1, ALevel1)     -- ExOC_ID SystemID
--     , (TagEventFailure_APO2, ALevel1)     -- ExOC_ID SystemID
--     , (TagEventFailure_APO3, ALevel1)     -- ExOC_ID SystemID
--     , (TagEventFailure_CE, ALevel1)       -- CE_ID
--     , (TagEventFailure_FAN, ALevel1)      -- ExOC_ID
--     , (TagEventMajorFailure_OC, ALevel1)  -- ExOC_ID
--     , (TagEventFailure_HRUnableToDeEnergized, ALevel1) -- SignalID
--     , (TagEventFailure_HRUnableToEnergized, ALevel1) -- SignalID
--     , (TagEventFailure_WLRUnlocked, ALevel1) -- PointID
--     , (TagEventFailure_WLRLocked, ALevel1) -- PointID
--     , (TagEventFailure_PointUnableToSwitch, ALevel1) -- PointID
--     , (TagEventFailure_PointAbnormalPosition00, ALevel1) -- PointID
--     , (TagEventFailure_PointAbnormalPosition11, ALevel1) -- PointID
--     , (TagEventFailure_EKTFailure, ALevel1) -- MA_ID
--     , (TagEventFailure_SPKFailure, ALevel1) -- SPK_ID
--     , (TagEventFailure_PSDFailure1, ALevel1) -- StopPointCode
--     , (TagEventFailure_PSDFailure2, ALevel1) -- StopPointCode
--         -- ATPGroundFailureEvent ATPGroundFailureEvent SC_ID
--     , (TagEventMinorFailure_SC, ALevel1)
--     , (TagEventFailure_100MLAN_R, ALevel1) -- SystemID
--     , (TagEventFailure_100MLAN_L, ALevel1) -- SystemID
--     , (TagEventFailure_SRAM, ALevel1) -- SystemID
--     , (TagEventFailure_NeighbouringSC, ALevel1) -- SystemID
--     , (TagEventFailure_ASIOCommLoss, ALevel1) -- SystemID
--     , (TagEventFailure_ACEHCommLoss, ALevel1) -- SystemID
--     , (TagEventFailure_100MLAN, ALevel1) -- SystemID
--     , (TagEventFailure_SRS, ALevel1) -- SystemID
--     , (TagEventFailure_5V2ALM1, ALevel1) -- SystemID
--     , (TagEventFailure_5V1ALM1, ALevel1) -- SystemID
--     , (TagEventFailure_24V2ALM1, ALevel1) -- SystemID
--     , (TagEventFailure_24V1ALM1, ALevel1) -- SystemID
--     , (TagEventFailure_WRS, ALevel1) -- WRS_ID
--     , (TagEventMajorFailure_SC, ALevel1)        -- SC_ID
--         -- ATPOnBoardFailureEvent ATPOnBoardFailureEvent (RakeID, Maybe Location) [SC_ID]
--     , (TagEventMinorFailure_VOBC, ALevel1)
--     , (TagEventFailure_LogicBlock, ALevel1) -- SystemID
--     , (TagEventFailure_IFBlock, ALevel1) -- SystemID
--     , (TagEventFailure_RelayBlock, ALevel1) -- SystemID
--     , (TagEventFailure_PWM, ALevel1) -- SystemID
--     , (TagEventFailure_TCMS, ALevel1) -- SystemID
--     , (TagEventFailure_VRS, ALevel1) -- VRSErrorCode SystemID
--     , (TagEventFailure_PG, ALevel1) -- SystemID
--     , (TagEventFailure_DMI, ALevel1) -- SystemID
--     , (TagEventFailure_BaliseAntenna, ALevel1) -- SystemID
--     , (TagEventMajorFailure_VOBC, ALevel1)
--         -- NetworkFailureEvent NetworkFailureEvent
--     , (TagEventNoCommunicationSingleNetworkWithServer, ALevel1) -- ServerID NetworkID
--     , (TagEventNoCommunicationSingleNetworkWithSC, ALevel1) -- SC_ID SystemID NetworkID
--     , (TagEventNoCommunicationSingleNetworkWithOC, ALevel1) -- OC_ID SystemID NetworkID
--     , (TagEventNoCommunicationSingleNetworkWithWorkstation, ALevel1) -- WorkstationID NetworkID
--     -- ATS Network Major Failure
--     , (TagEventNoCommunicationWithServer, ALevel1) -- ServerID
--     , (TagEventNoCommunicationWithSC, ALevel1) -- SC_ID SystemID
--     , (TagEventNoCommunicationWithOC, ALevel1) -- OC_ID SystemID
--     , (TagEventNoCommunicationWithWorkstation, ALevel1) -- WorkstationID
--     , (TagEventNoCommunicationWithPA, ALevel1)
--     , (TagEventNoCommunicationWithTrainRadio, ALevel1)
--     , (TagEventNoCommunicationWithPSD, ALevel1) -- (StationCode, PlatformNumber)
--     , (TagEventNoCommunicationWithUPS, ALevel1) -- StationCode
--     , (TagEventNoCommunicationWithTCMS, ALevel1) -- RakeID
--     , (TagEventNoCommunicationWithTVS, ALevel1)]
