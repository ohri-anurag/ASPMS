{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Data.Array
import Data.Monoid((<>))
import Control.Monad.Trans
import Control.Concurrent
import Control.Exception.Base(catch, SomeException(..))
import Data.IORef
import qualified Data.Text as T
import qualified Data.ByteString as B
import Text.Read(readMaybe)
import System.IO(stdout)
-- import Network.Socket(withSocketsDo)

import SP6.Data.ID
import SP6.CommonIO hiding (accountFilePath)

-- Provides HTML templates
import qualified Html as H

-- Data Source
import AccountDataSource
import Credentials
import Network
import Types
import Utility

data MyAppState = MyAppState
    { state :: IORef AppState
    , cache :: IORef AccountAndSystemParameterConfig
    }

data AppState = InUse | Free
    deriving Show

data UserSession = UserLoggedIn | UserLoggedOut
    deriving Show

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
    debugMain "Receiving server health..."
    (arrServerStatus, _) <- initReceiverServerStatus handle False []
    debugMain "Receiving workstation health..."
    (arrWorkstationStatus, _) <- initReceiverWorkstationStatus handle False []
    -- arrServerStatus <- initMVarArray (minBound, maxBound) (-1, -1)
    -- arrWorkstationStatus <- initMVarArray (minBound, maxBound) (-1, -1)

    -- Initialize a spock instance
    stateRef <- newIORef Free
    debugMain "Reading saved account data from disk..."
    acData <- getData
    dataRef <- newIORef acData
    spockCfg <- defaultSpockCfg UserLoggedOut PCNoDatabase (MyAppState stateRef dataRef)
    debugMain "Initializing Spock Instance"
    runSpock 8080 $ spock spockCfg $ app accountBytesRef arrServerStatus arrWorkstationStatus

app
    :: IORef B.ByteString
    -> Array ServerID (MVar (Int, Int))
    -> Array WorkstationID (MVar (Int, Int))
    -> SpockM () UserSession MyAppState ()
app accountBytesRef arrServerStatus arrWorkstationStatus = do
    get root $
        redirect "/login"

    get "login" $
        userAuthenticated (redirect "/home") (html H.login)
    post "login" $
        userAuthenticated (redirect "/home") (do
            ps <- paramsPost
            maybe (redirect "/login") (\pwd -> do
                b <- runQuery $ const $ do
                    debugMain "Validating password..."
                    validatePassword pwd
                if b
                    then login >> redirect "/home"
                    else redirect "/login"
                ) (lookup "password" ps)
            )

    get "logout" $
        logout >> redirect "/login"

    get "home" $
        userAuthenticated (withData H.home) (redirect "/login")

    get "changePassword" $
        userAuthenticated (html H.changePassword) (redirect "/login")
    post "changePassword" $
        userAuthenticated (do
            ps <- paramsPost
            maybe (text "0") (\pwd -> do
                runQuery $ const $ do
                    debugMain "Storing new password..."
                    storePassword pwd
                text "1"
                ) (lookup "password" ps)
            ) (redirect "/login")

    get ("account" <//> var) $ \uid ->
        -- TODO: This will have to be changed when the UserID type changes.
        userAuthenticated (withData $ H.editAccount uid) (redirect "/login")
    post "account" $
        userAuthenticated (updateCacheWith updateAccount) (redirect "/login")

    get "addAccount" $
        userAuthenticated (withData $ H.addAccount) (redirect "/login")
    post "addAccount" $
        userAuthenticated (updateCacheWith updateAccount) (redirect "/login")

    post "deleteAccount" $
        userAuthenticated (updateCacheWith deleteAccount) (redirect "/login")

    post "systemParams" $
        userAuthenticated (updateCacheWith updateSystemParams) (redirect "/login")

    get "runningTimeLists" $
        userAuthenticated (withData H.runningTimeLists) (redirect "/login")
    post "runningTimeLists" $
        userAuthenticated (updateCacheWith updateRunningTimeLists) (redirect "/login")

    get "dwellTimeSets" $
        userAuthenticated (withData H.dwellTimeSets) (redirect "/login")
    post "dwellTimeSets" $
        userAuthenticated (updateCacheWith updateDwellTimeSets) (redirect "/login")

    get "alarmLevels" $
        userAuthenticated (withData H.alarmLevels) (redirect "/login")
    post "alarmLevels" $
        userAuthenticated (updateCacheWith updateAlarmLevels) (redirect "/login")

    post "applyChanges" $
        userAuthenticated (do
            val <- runQuery $ const $ catch (do
                -- Read latest account data
                debugMain "Getting saved data from file..."
                accountBytes <- getDataBytes accountFilePath

                -- Update the accountDataRef
                debugMain "Updating the cache..."
                atomicModifyIORef' accountBytesRef $ const (accountBytes, ())

                -- Update the copy
                debugMain "Updating the applied data..."
                B.writeFile accountFileCopy accountBytes

                -- Send update commands to all workstations and servers
                debugMain "Sending update command to all..."
                sendUpdateCommands arrServerStatus arrWorkstationStatus

                -- Send success code
                debugMain "Sending success code..."
                pure "1"
                ) errorHandler
            text val
            ) (redirect "/login")

    where
        errorHandler :: SomeException -> IO T.Text
        errorHandler e = do
            putStrLn $ "Encountered error : " ++ show e
            pure "0"

        -- Read account data from the cache, and run the action with that data
        withData :: (AccountAndSystemParameterConfig -> T.Text) -> SpockAction () UserSession MyAppState ()
        withData action = do
            (MyAppState _ cache) <- getState
            acData <- liftIO (readIORef cache)
            html $ action acData

        updateCacheWith :: ([(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig)
            -> SpockAction () UserSession MyAppState ()
        updateCacheWith update = do
            ps <- paramsPost
            (MyAppState _ cache) <- getState
            acData <- liftIO (readIORef cache)
            maybe (text "0") (updateCache cache) $ update ps acData
            where
                updateCache :: IORef AccountAndSystemParameterConfig -> AccountAndSystemParameterConfig
                    -> SpockAction () UserSession MyAppState ()
                updateCache cache newData = do
                    runQuery $ const $ do
                        debugMain "Changes saved..."
                        putData newData
                        atomicModifyIORef' cache $ const (newData,())
                    text "1"

        -- Check if this user is logged in. If yes, perform the given action, otherwise redirect to the login page.
        userAuthenticated actionTrue actionFalse = do
            session <- readSession
            case session of
                UserLoggedIn -> actionTrue
                UserLoggedOut -> do
                  -- Check if some other user is already using the app. If yes, don't show the login page.
                    (MyAppState ref _) <- getState
                    state <- liftIO (readIORef ref)
                    case state of
                        InUse -> text "The account management system is in use by another user. Kindly login at a later time."
                        Free -> actionFalse

        login = writeAppState InUse >> writeSession UserLoggedIn
        logout = writeAppState Free >> writeSession UserLoggedOut

        writeAppState :: AppState -> SpockAction () UserSession MyAppState ()
        writeAppState state = do
            (MyAppState ref _) <- getState
            liftIO $ atomicModifyIORef' ref (\_ -> (state, ()))
