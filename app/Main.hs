{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Data.Monoid((<>))
import Control.Monad.Trans
import Data.IORef
import qualified Data.Text as T

-- Provides HTML templates
import qualified Html as H

-- Data Source
import AccountDataSource
import Credentials
import Text.Read(readMaybe)

data MyAppState = MyAppState
    { state :: IORef AppState
    , cache :: IORef AccountAndSystemParameterConfig
    }

data AppState = InUse | Free
    deriving Show

data UserSession = UserLoggedIn | UserLoggedOut
    deriving Show

-- TODO Change the port
main :: IO ()
main = do
    ref <- newIORef Free
    dataRef <- getData >>= newIORef
    spockCfg <- defaultSpockCfg UserLoggedOut PCNoDatabase (MyAppState ref dataRef)
    runSpock 8080 (spock spockCfg app)

app :: SpockM () UserSession MyAppState ()
app = do
    get root $
        redirect "/login"

    get "login" $
        userAuthenticated (redirect "/home") (html H.login)
    post "login" $
        userAuthenticated (redirect "/home") (paramsPost >>= \ps ->
            case lookup "password" ps of
                Nothing -> redirect "/login"
                Just pwd -> (runQuery $ \_ -> do
                    validatePassword pwd) >>= \b -> if b
                        then login >> redirect "/home"
                        else redirect "/login"
        )

    get "logout" $
        logout >> redirect "/login"

    get "home" $
        userAuthenticated (withData H.home) (redirect "/login")

    get "changePassword" $
        userAuthenticated (html H.changePassword) (redirect "/login")
    post "changePassword" $
        userAuthenticated (paramsPost >>= \ps ->
            case lookup "password" ps of
                Nothing -> text "0"
                Just pwd -> (runQuery $ \_ -> do
                    storePassword pwd) >> text "1"
        ) (redirect "/login")

    get ("account" <//> var) $ \uid ->
        -- TODO This will have to be changed when the UserID type changes.
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

    where
        -- Read account data from the cache, and run the action with that data
        withData :: (AccountAndSystemParameterConfig -> T.Text) -> SpockAction () UserSession MyAppState ()
        withData action = do
            (MyAppState _ cache) <- getState
            liftIO (readIORef cache) >>= html . action

        updateCacheWith :: ([(T.Text, T.Text)] -> AccountAndSystemParameterConfig -> Maybe AccountAndSystemParameterConfig)
            -> SpockAction () UserSession MyAppState ()
        updateCacheWith update = paramsPost >>= \ps -> do
            (MyAppState _ cache) <- getState
            acData <- liftIO (readIORef cache)
            case update ps acData of
                Just acData' -> (runQuery $ \_ -> do
                    putData acData'
                    atomicModifyIORef' cache (\_ -> ( acData',() ))) >> text "1"
                Nothing -> text "0"

        -- Check if this user is logged in. If yes, perform the given action, otherwise redirect to the login page.
        userAuthenticated actionTrue actionFalse = readSession >>= \session ->
            case session of
                UserLoggedIn -> actionTrue
                UserLoggedOut -> do
                  -- Check if some other user is already using the app. If yes, don't show the login page.
                    (MyAppState ref _) <- getState
                    liftIO (readIORef ref) >>= \state -> case state of
                        InUse -> text "The account management system is in use by another user. Kindly login at a later time."
                        Free -> actionFalse

        login = writeAppState InUse >> writeSession UserLoggedIn
        logout = writeAppState Free >> writeSession UserLoggedOut

        writeAppState :: AppState -> SpockAction () UserSession MyAppState ()
        writeAppState state = do
            (MyAppState ref _) <- getState
            liftIO $ atomicModifyIORef' ref (\_ -> (state, ()))
