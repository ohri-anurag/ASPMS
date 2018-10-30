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
    updateData,
    updateRollingStockRoster,
    updateCrewRoster,
    AccountAndSystemParameterConfig
) where

-- Data Type Definitions
import SP6.Data.Account
import SP6.Data.ID

import Types()
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
        debugMain $ "Could not read data from file. Resorting to default values. Error : " ++ show e
        pure $ S.encode (def :: AccountAndSystemParameterConfig)

getData :: IO AccountAndSystemParameterConfig
getData = either (const def) id . S.decode
    <$> getDataBytes accountFilePath

putData :: AccountAndSystemParameterConfig -> IO ()
putData accountData = B.writeFile accountFilePath (S.encode accountData)

-- Assumption that JSON data is being sent to the server.
createAccount :: [(T.Text, T.Text)] -> Maybe (UserID2, Account)
createAccount paramList = do
    text <- lookup "userID" paramList
    accountJson <- lookup "data" paramList
    acc <- decodeStrict' $ TE.encodeUtf8 accountJson
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
modifyAccount (uid, acc) (AccountAndSystemParameterConfig sysParam accConf) = AccountAndSystemParameterConfig sysParam (M.insert uid acc accConf)

updateAccount :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateAccount ps accConfSysParam = flip modifyAccount accConfSysParam <$> createAccount ps
    -- FOR DEBUGGING
    -- case createAccount ps of
    --     Nothing -> error "Could not create account"
    --     Just x -> Just (modifyAccount x accConf)

deleteAccount :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
deleteAccount ps (AccountAndSystemParameterConfig sysParam accConf) =
    AccountAndSystemParameterConfig sysParam . delete
    <$> lookup "userID" ps
    where
        delete uid = M.delete (UserID2 $ T.unpack uid) accConf

updateSystemParams :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateSystemParams ps accConfSysParam = do
    departureOffsetVal <- get "departureOffset"
    routeTriggerOffsetVal <- get "routeTriggerOffset"
    minimumDwellTimeVal <- get "minimumDwellTime"
    delayDetectionThreshHoldVal <- get "delayDetectionThreshHold"
    intestationStopDetectionTimeVal <- get "interstationStopDetectionTime"
    wakeUpCommandOffsetVal <- get "wakeUpCommandOffset"
    tunnelLimitVal <- lookup "tunnelLimit" ps >>= readMaybe . T.unpack
    Just $ accConfSysParam {
        systemParameter = (systemParameter accConfSysParam) {
            departureOffset = departureOffsetVal,
            routeTriggerOffset = routeTriggerOffsetVal,
            minimumDwellTime = minimumDwellTimeVal,
            delayDetectionThreshHold = delayDetectionThreshHoldVal,
            interstationStopDetectionTime = intestationStopDetectionTimeVal,
            wakeUpCommandOffset = wakeUpCommandOffsetVal,
            tunnelLimit = tunnelLimitVal
        }
    }
    where
        get key = do
            keyJson <- lookup key ps
            decodeStrict' $ TE.encodeUtf8 keyJson

updateRunningTimeLists :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateRunningTimeLists ps accConfSysParam = do
    rtlJson <- lookup "data" ps
    runningTimeList <- decodeStrict' $ TE.encodeUtf8 rtlJson
    Just $ accConfSysParam {
        systemParameter = (systemParameter accConfSysParam) {
            runningTimeLists = runningTimeList
        }
    }

updateDwellTimeSets :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateDwellTimeSets ps accConfSysParam = do
    dtsJson <- lookup "data" ps
    dwellTimeSet <- decodeStrict' $ TE.encodeUtf8 dtsJson
    Just $ accConfSysParam {
        systemParameter = (systemParameter accConfSysParam) {
            dwellTimeSets = dwellTimeSet
        }
    }

updateAlarmLevels :: [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateAlarmLevels ps accConfSysParam = do
    alJson <- lookup "data" ps
    alarmLevelVal <- decodeStrict' $ TE.encodeUtf8 alJson
    Just $ accConfSysParam {
        systemParameter = (systemParameter accConfSysParam) {
            alarmLevel = alarmLevelVal
        }
    }

updateData :: B.ByteString -> [(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig
updateData accountData _ _ = either (const Nothing) Just $ S.decode accountData

updateRollingStockRoster
    :: [(T.Text, T.Text)]
    -> AccountAndSystemParameterConfig
    -> Maybe AccountAndSystemParameterConfig
updateRollingStockRoster ps accConfSysParam = do
    rsJson <- lookup "data" ps
    rollingStock <- decodeStrict' $ TE.encodeUtf8 rsJson
    Just $ accConfSysParam {
        systemParameter = (systemParameter accConfSysParam) {
            rollingStockRoster = rollingStock
        }
    }

updateCrewRoster
    :: [(T.Text, T.Text)]
    -> AccountAndSystemParameterConfig
    -> Maybe AccountAndSystemParameterConfig
updateCrewRoster ps accConfSysParam = do
    crJson <- lookup "data" ps
    crew <- decodeStrict' $ TE.encodeUtf8 crJson
    Just $ accConfSysParam {
        systemParameter = (systemParameter accConfSysParam) {
            crewRoster = crew
        }
    }
