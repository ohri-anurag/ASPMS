{-# LANGUAGE OverloadedStrings #-}
module Credentials where

import Utility

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Control.Exception.Base(SomeException, catch)

defaultPassword :: T.Text -> T.Text
defaultPassword "crew"  = "passwordcrew"
defaultPassword "rsc"   = "passwordrsc"
defaultPassword _       = "password"

credentialsPath :: T.Text -> String
credentialsPath "crew"  = credentialsPathCrew
credentialsPath "rsc"   = credentialsPathRSC
credentialsPath _       = credentialsPathCC

-- TODO: Role Selector here as well
storePassword :: T.Text -> IO ()
storePassword = B.writeFile credentialsPathCC . TE.encodeUtf8

validatePassword :: T.Text -> T.Text -> IO Bool
validatePassword role text = (==) text . TE.decodeUtf8 <$> catch (B.readFile $ credentialsPath role) handler
    where
        handler :: SomeException -> IO B.ByteString
        handler e = do
            debugMain $ "Could not read password from file. Resorting to default password. Error : " ++ show e
            pure $ TE.encodeUtf8 $ defaultPassword role
