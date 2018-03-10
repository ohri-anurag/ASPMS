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
import Text.Read(readMaybe)

data MyAppState = MyAppState
    { state :: IORef AppState
    , cache :: IORef AccountAndSystemParameterConfig
    }


data AppState = InUse | Free
    deriving Show

data UserSession = UserLoggedIn | UserLoggedOut
    deriving Show

toggle :: AppState -> (AppState, AppState)
toggle Free = (InUse, InUse)
toggle InUse = (Free, Free)

main :: IO ()
main = do
    ref <- newIORef Free
    dataRef <- getData >>= newIORef
    spockCfg <- defaultSpockCfg UserLoggedOut PCNoDatabase (MyAppState ref dataRef)
    runSpock 8080 (spock spockCfg app)

validatePassword :: [(T.Text, T.Text)] -> Bool
validatePassword parameters = case lookup "password" parameters of
    Nothing -> False
    Just pw -> pw == "simple"

app :: SpockM () UserSession MyAppState ()
app = do
    get root $
        redirect "/login"

    get "login" $
        userAuthenticated (redirect "/home") (html H.login)

    get "logout" $
        toggleAppState >> writeSession UserLoggedOut >> redirect "/login"

    get "home" $
        userAuthenticated (withData H.home) (redirect "/login")

    get ("account" <//> var) $ \uid ->
        -- TODO This will have to be changed when the UserID type changes.
        case readMaybe uid of
            Just x -> userAuthenticated (withData $ H.editAccount x) (redirect "/login")
            Nothing -> error "Invalid UserID"

    post "account" $
        userAuthenticated (
            paramsPost >>= \ps -> do
                (MyAppState _ cache) <- getState
                acData <- liftIO (readIORef cache)
                case updateAccount ps acData of
                    Just acData' -> (runQuery $ \_ -> do
                        putData acData'
                        atomicModifyIORef' cache (\_ -> ( acData',() ))) >> text "1"
                    Nothing -> text "0"
        ) (redirect "/login")

    get "addAccount" $
        userAuthenticated (withData $ H.addAccount) (redirect "/login")

    get "runningTimeLists" $
        userAuthenticated (withData H.runningTimeLists) (redirect "/login")

    get "dwellTimeSets" $
        userAuthenticated (withData H.dwellTimeSets) (redirect "/login")

    get "alarmLevels" $
        userAuthenticated (withData H.alarmLevels) (redirect "/login")

    post "login" $ paramsPost >>= \ps ->
        if validatePassword ps
            then toggleAppState >> writeSession UserLoggedIn >> redirect "/home"
            else redirect "/login"
    where
        -- Read account data from the cache, and run the action with that data
        withData :: (AccountAndSystemParameterConfig -> T.Text) -> SpockAction () UserSession MyAppState ()
        withData action = do
            (MyAppState _ cache) <- getState
            liftIO (readIORef cache) >>= html . action

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

        toggleAppState = do
            (MyAppState ref _) <- getState
            liftIO $ atomicModifyIORef' ref toggle

-- TODO
-- Fix toggleAppState, state should not be toggled, instead it should be set to whatever is apt
