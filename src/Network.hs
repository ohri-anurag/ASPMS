module Network where

import Types

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv, sendTo)
import Network.Socket.ByteString (recv, sendTo)
import Control.Monad(void)

sendFile :: IO ()
sendFile = pure ()

sendUpdateCommand :: IO ()
sendUpdateCommand = withSocketsDo $ do
    E.bracket open close talk
    where
    open = do
        sock <- socket AF_INET Datagram defaultProtocol
        setSocketOption sock Broadcast 1
        pure sock
    talk sock = void
        $ sendTo sock encodedCommand
        -- TODO Finalise the IP and the port
        $ SockAddrInet 3000
        $ tupleToHostAddress (255,255,255,255)