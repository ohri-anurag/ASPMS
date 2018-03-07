{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
-- import Data.Monoid
import Data.IORef
import qualified Data.Text as T

-- Provides the data for all accounts
import AccountDataSource
-- Provides HTML templates
import qualified Html as H

data MyAppState = MyAppState (IORef AppState)


data AppState = InUse | Free
    deriving Show

data UserSession = UserLoggedIn | UserLoggedOut
    deriving Show

toggle :: AppState -> (AppState, AppState)
toggle Free = (InUse, InUse)
toggle InUse = (Free, Free)

main :: IO ()
main =
    do ref <- newIORef Free
       spockCfg <- defaultSpockCfg UserLoggedOut PCNoDatabase (MyAppState ref)
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
    -- userAuthenticated (redirect "/home") (file "text/html" "res/Login.html")
        userAuthenticated (redirect "/home") (html H.login)

    get "logout" $
        toggleAppState >> writeSession UserLoggedOut >> redirect "/login"

    get "home" $
        userAuthenticated (html H.home) (redirect "/login")

    get ("account" <//> var) $ \uid ->
        userAuthenticated (html $ H.account uid) (redirect "/login")

    post "login" $ paramsPost >>= \ps ->
        if validatePassword ps
            then toggleAppState >> writeSession UserLoggedIn >> redirect "/home"
            else redirect "/login"
    where
        -- Check if this user is logged in. If yes, perform the given action, otherwise redirect to the login page.
        userAuthenticated actionTrue actionFalse = readSession >>= \session ->
            case session of
                UserLoggedIn -> actionTrue
                UserLoggedOut -> do
                  -- Check if some other user is already using the app. If yes, don't show the login page.
                    (MyAppState ref) <- getState
                    liftIO (readIORef ref) >>= \state -> case state of
                        InUse -> text "The account management system is in use by another user. Kindly login at a later time."
                        Free -> actionFalse

        toggleAppState = do
            (MyAppState ref) <- getState
            liftIO $ atomicModifyIORef' ref toggle

-- TODO
-- Fix toggleAppState, state should not be toggled, instead it should be set to whatever is apt
