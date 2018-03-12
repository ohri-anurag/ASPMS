module Credentials where

-- validatePassword ::
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

storePassword :: T.Text -> IO ()
storePassword = B.writeFile "data/Credentials" . TE.encodeUtf8

validatePassword :: T.Text -> IO Bool
validatePassword text = ((==) text . TE.decodeUtf8) <$> B.readFile "data/Credentials"
