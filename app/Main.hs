{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Data.Monoid((<>))
import Control.Monad.Trans
import Control.Exception.Base(catch, SomeException(..))
import Data.IORef
import qualified Data.Text as T

-- Provides HTML templates
import qualified Html as H

-- Data Source
import AccountDataSource
import Credentials
import Network
import Text.Read(readMaybe)

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
main = do
    -- Initialize a spock instance
    ref <- newIORef Free
    acData <- getData
    dataRef <- newIORef acData
    spockCfg <- defaultSpockCfg UserLoggedOut PCNoDatabase (MyAppState ref dataRef)
    runSpock 8080 (spock spockCfg app)

    -- Initialize a TCP server for receiving requests for AccountConfig
    initTCPServer

app :: SpockM () UserSession MyAppState ()
app = do
    get root $
        redirect "/login"

    get "login" $
        userAuthenticated (redirect "/home") (html H.login)
    post "login" $
        userAuthenticated (redirect "/home") (do
            ps <- paramsPost
            maybe (redirect "/login") (\pwd -> do
                b <- runQuery $ const $ validatePassword pwd
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
                runQuery $ const $ storePassword pwd
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
                sendUpdateCommands
                pure "1"
                ) (\(SomeException e) -> do
                    print e
                    pure "0") -- :: SomeException -> IO T.Text)
            text val
            ) (redirect "/login")

    where
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
