module Credentials where

import Utility

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Control.Exception.Base(SomeException, catch)

storePassword :: T.Text -> IO ()
storePassword = B.writeFile "data/Credentials" . TE.encodeUtf8

validatePassword :: T.Text -> IO Bool
validatePassword text = ((==) text . TE.decodeUtf8) <$> catch (B.readFile "data/Credentials") handler
    where
        handler :: SomeException -> IO B.ByteString
        handler e = do
            debugMain $ "Could not read password from file. Resorting to default password. Error : " ++ (show e)
            pure $ TE.encodeUtf8 $ T.pack "password"
