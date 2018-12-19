{-# LANGUAGE OverloadedStrings #-}
module Credentials where

import Utility
import Types

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Control.Exception.Base(SomeException, catch)

defaultPassword :: User -> T.Text
defaultPassword CrewController          = "passwordcrew"
defaultPassword RollingStockController  = "passwordrsc"
defaultPassword _                       = "password"

credentialsPath :: User -> String
credentialsPath CrewController          = credentialsPathCrew
credentialsPath RollingStockController  = credentialsPathRSC
credentialsPath _                       = credentialsPathCC

roleToUser :: T.Text -> User
roleToUser "crew"   = CrewController
roleToUser "rsc"    = RollingStockController
roleToUser _        = ChiefController

storePassword :: User -> T.Text -> IO ()
storePassword user = B.writeFile (credentialsPath user) . TE.encodeUtf8

validatePassword :: User -> T.Text -> IO Bool
validatePassword user text = (==) text . TE.decodeUtf8 <$> catch (B.readFile $ credentialsPath user) handler
    where
        handler :: SomeException -> IO B.ByteString
        handler e = do
            debugMain $ "Could not read password from file. Resorting to default password. Error : " ++ show e
            pure $ TE.encodeUtf8 $ defaultPassword user
