{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Data.Array
import Data.Word(Word32)
import Control.Monad(void, forM_, when)
import Control.Monad.Trans
import Control.DeepSeq
import Control.Concurrent
import Control.Exception.Base(catch, SomeException(..))
import Data.IORef
import qualified Data.Text as T
import qualified Data.ByteString as B
import System.IO(stdout)
import Data.Derive.Class.Default
import Data.Time.Clock.POSIX(getPOSIXTime)
import Data.Monoid ((<>))

import SP6.Data.ID hiding (RollingStockController)
import SP6.CommonIO
import SP6.Data.Render
import SP6.Data.Utility
import SP6.Data.Common

-- Provides HTML templates
import qualified Html as H

-- Data Source
import Types
import AccountDataSource
import Credentials
import Network
-- import Types
import Utility

data MyAppState = MyAppState
    { cache :: IORef AccountAndSystemParameterConfig
    , health :: IORef Int
    }

data UserSession = UserLoggedIn User | UserLoggedOut
    deriving Show

-- The interval(seconds) after which user is logged out
healthInterval :: Int
healthInterval = 60

-- TODO: Change the port
main :: IO ()
main = withSocketsDo $ do
    -- Initialize a TCP server for receiving requests for AccountConfig data
    debugMain "Reading applied account data from disk..."
    accountBytes <- getDataBytes accountFileCopy
    accountBytesRef <- newIORef accountBytes

    debugMain "Initializing TCP Server..."
    initTCPServer accountBytesRef

    -- Initialize a UDP server for sending heartbeat requests
    debugMain "Initializing Heartbeat server..."
    initHeartbeatServer

    -- Initialize Health receivers for both servers and workstations
    handle <- newMVar stdout
    loginStatus <- newMVar $ Logout def
    debugMain "Receiving server health..."
    (arrServerStatus, _) <- initReceiverServerStatus handle loginStatus []
    debugMain "Receiving workstation health..."
    (arrWorkstationStatus, _) <- initReceiverWorkstationStatus handle loginStatus []

    -- Decrement health information every 500 ms
    void $ forkIO $ addTimer 500000 $
        let
            modifyMVarArrayPure :: (NFData a, Enum i, Ix i, Bounded i) => Array i (MVar a) -> (a -> a) -> IO ()
            modifyMVarArrayPure arr f = forM_ allElems $ \ x ->
                modifyMVarPure_ (arr <! x) f
        in do
            modifyMVarArrayPure arrServerStatus $ makePair (mapPair pred . fst) snd
            modifyMVarArrayPure arrWorkstationStatus $ makePair (mapPair pred . fst) snd

    -- Start decrementing heartbeat every second
    healthRef <- newIORef healthInterval
    void $ forkIO $ addTimer 1000000 $
        -- Do not decrement less than 0
        atomicModifyIORef' healthRef $ \ h ->
            (if h == 0 then 0 else h - 1, ())

    -- Initialize a spock instance
    debugMain "Reading saved account data from disk..."
    acData <- getData
    dataRef <- newIORef acData
    spockCfg <- defaultSpockCfg UserLoggedOut PCNoDatabase (MyAppState dataRef healthRef)
    debugMain "Initializing Spock Instance"
    runSpock 8080 $ spock spockCfg $ app accountBytesRef arrServerStatus arrWorkstationStatus

app :: IORef B.ByteString
    -> Array ServerID (MVar ((Int, Int), Word32))
    -> Array WorkstationID (MVar ((Int, Int), Word32))
    -> SpockM () UserSession MyAppState ()
app accountBytesRef arrServerStatus arrWorkstationStatus = do
    get "crew" $
        userAuthenticated (withData H.crewRosterPage) (redirectUserToLogin "crew")
    post "crew" $
        userAuthenticated (updateCacheWith updateCrewRoster) (redirectUserToLogin "crew")

    get "rsc" $
        userAuthenticated (withData H.rollingStockRosterPage) (redirectUserToLogin "rsc")
    post "rsc" $
        userAuthenticated (updateCacheWith updateRollingStockRoster) (redirectUserToLogin "rsc")

    let getLogin :: User -> SpockAction () UserSession MyAppState ()
        getLogin user = userAuthenticated (redirectLoggedUserToHome user) (html $ H.login user)

    get ("login" <//> var) (getLogin . roleToUser)
    post ("login" <//> var) $ \role -> do
        let user = roleToUser role
        userAuthenticated (redirectLoggedUserToHome user) (do
            ps <- paramsPost
            maybe (redirectUserToLogin role) (\pwd -> do
                b <- runQuery $ const $ do
                    debugMain "Validating password..."
                    validatePassword user pwd
                if b
                    then do
                        loginUser user
                        updateHealth
                        redirectLoggedUserToHome user
                    else redirectUserToLogin role
                ) (lookup "password" ps)
            )

    get ("logout" <//> var) $ \role ->
        logout >> redirectUserToLogin role

    get "heartbeat" updateHealth

    get "home" $
        userAuthenticated (withData H.home) (redirectUserToLogin "cc")

    get ("changePassword" <//> var) $ \role ->
        userAuthenticated (html $ H.changePassword $ roleToUser role) (redirectUserToLogin role)
    post ("changePassword" <//> var) $ \role ->
        userAuthenticated (do
            ps <- paramsPost
            maybe (text "0") (\pwd -> do
                runQuery $ const $ do
                    debugMain "Storing new password..."
                    let user = roleToUser role
                    storePassword user pwd
                text "1"
                ) (lookup "password" ps)
            ) (redirectUserToLogin role)

    get ("account" <//> var) $ \uid ->
        -- TODO: This will have to be changed when the UserID type changes.
        userAuthenticated (withData $ H.editAccount uid) (redirectUserToLogin "cc")
    post "account" $
        userAuthenticated (updateCacheWith updateAccount) (redirectUserToLogin "cc")

    get "addAccount" $
        userAuthenticated (withData H.addAccount) (redirectUserToLogin "cc")
    post "addAccount" $
        userAuthenticated (updateCacheWith updateAccount) (redirectUserToLogin "cc")

    post "deleteAccount" $
        userAuthenticated (updateCacheWith deleteAccount) (redirectUserToLogin "cc")

    get "saveData" $
        userAuthenticated (do
            val <- runQuery $ const $ catch (do
                -- Read latest account data
                debugMain "Getting current data from file..."
                accountBytes <- getDataBytes accountFilePath

                -- Get the current time
                debugMain "Get the current time..."
                time <- getPOSIXTime

                let roundTime = round (realToFrac time :: Double) :: Integer
                let fileName = "SavedData_" ++ show roundTime

                -- Save the current account and system data onto disk
                debugMain $ "Save current account data to file" ++ fileName
                B.writeFile fileName accountBytes

                -- Create a JSON string to be sent to the client side
                let str = T.pack $
                        "{\"fileName\":\"" ++ fileName ++ "\",\"data\":" ++ show (B.unpack accountBytes) ++ "}"
                pure str
                ) errorHandler
            text val
            ) (redirectUserToLogin "cc")

    post "loadData" $
        userAuthenticated (body >>= \fileData -> updateCacheWith (updateData fileData)) (redirectUserToLogin "cc")

    post "systemParams" $
        userAuthenticated (updateCacheWith updateSystemParams) (redirectUserToLogin "cc")

    get "runningTimeLists" $
        userAuthenticated (withData H.runningTimeListsPage) (redirectUserToLogin "cc")
    post "runningTimeLists" $
        userAuthenticated (updateCacheWith updateRunningTimeLists) (redirectUserToLogin "cc")

    get "dwellTimeSets" $
        userAuthenticated (withData H.dwellTimeSetsPage) (redirectUserToLogin "cc")
    post "dwellTimeSets" $
        userAuthenticated (updateCacheWith updateDwellTimeSets) (redirectUserToLogin "cc")

    get "alarmLevels" $
        userAuthenticated (withData H.alarmLevels) (redirectUserToLogin "cc")
    post "alarmLevels" $
        userAuthenticated (updateCacheWith updateAlarmLevels) (redirectUserToLogin "cc")

    post "applyChanges" $
        userAuthenticated (do
            val <- runQuery $ const $ catch (do
                -- Read latest account data
                debugMain "Getting saved data from file..."
                accountBytes <- getDataBytes accountFilePath

                -- Get the old applied data, to create a timestamped copy
                oldBytes <- readIORef accountBytesRef

                -- Update the accountBytesRef
                debugMain "Updating the cache..."
                atomicModifyIORef' accountBytesRef $ const (accountBytes, ())

                -- Update the copy
                debugMain "Updating the applied data..."
                updateCopy oldBytes accountBytes

                -- Send update commands to all workstations and servers
                debugMain "Sending update command to all..."
                sendUpdateCommands arrServerStatus arrWorkstationStatus

                -- Send success code
                debugMain "Sending success code..."
                pure "1"
                ) errorHandler
            text val
            ) (redirectUserToLogin "cc")

    where
        errorHandler :: SomeException -> IO T.Text
        errorHandler e = do
            debugMain $ "Encountered error : " ++ show e
            pure "0"

        -- Read account data from the cache, and run the action with that data
        withData :: (AccountAndSystemParameterConfig -> T.Text) -> SpockAction () UserSession MyAppState ()
        withData action = do
            (MyAppState accountCache _) <- getState
            acData <- liftIO (readIORef accountCache)
            html $ action acData

        updateCacheWith :: ([(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig)
            -> SpockAction () UserSession MyAppState ()
        updateCacheWith update = do
            ps <- paramsPost
            (MyAppState accountCache _) <- getState
            acData <- liftIO (readIORef accountCache)
            maybe (text "0") (updateCache accountCache) $ update ps acData
            where
                updateCache :: IORef AccountAndSystemParameterConfig -> AccountAndSystemParameterConfig
                    -> SpockAction () UserSession MyAppState ()
                updateCache accountCache newData = do
                    runQuery $ const $ do
                        debugMain "Changes saved..."
                        putData newData
                        atomicModifyIORef' accountCache $ const (newData,())
                    text "1"

        -- Check if this user is logged in. If yes, perform the given action, otherwise redirect to the login page.
        userAuthenticated
            :: SpockAction () UserSession MyAppState ()
            -> SpockAction () UserSession MyAppState ()
            -> SpockAction () UserSession MyAppState ()
        userAuthenticated actionTrue actionFalse = do
            (MyAppState _ healthRef) <- getState
            healthVal <- liftIO $ readIORef healthRef
            when (healthVal == 0) logout
            session <- readSession
            case session of
                UserLoggedIn _ -> actionTrue
                UserLoggedOut -> actionFalse

        loginUser user = writeSession (UserLoggedIn user)
        logout = writeSession UserLoggedOut
        redirectUserToLogin role = redirect ("/login/" <> role)
        redirectLoggedUserToHome user = case user of
            CrewController          -> redirect "/crew"
            RollingStockController  -> redirect "/rsc"
            ChiefController         -> redirect "/home"

        updateHealth :: SpockAction () UserSession MyAppState ()
        updateHealth = do
            (MyAppState _ healthRef) <- getState
            liftIO $ atomicModifyIORef' healthRef $ const (healthInterval, ())
