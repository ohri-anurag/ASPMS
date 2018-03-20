module Network(
    sendFile,
    sendUpdateCommands
) where

import Types

-- TODO
-- Main.hs 3828
-- Use TCP/IP
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv, sendTo, send)
import Network.Socket.ByteString (recv, sendTo, send)
import Control.Monad(void)
import Data.Serialize
--
import SP6.Data.TimetableRegulation(withHealthyServers)

sendFile :: IO ()
sendFile = pure ()

serverIPs :: [HostAddress]
serverIPs = map tupleToHostAddress [(127,0,0,1)]

-- TODO Finalise the IP and the port
serverAddrs :: [SockAddr]
serverAddrs = map (SockAddrInet 3000) serverIPs

sendUpdateCommands :: IO ()
sendUpdateCommands = mapM_ sendUpdateCommand serverAddrs

sendUpdateCommand :: SockAddr -> IO Bool
sendUpdateCommand addr = withSocketsDo $ E.bracket open close talk
    where
    open = do
        sock <- socket AF_INET Stream defaultProtocol
        -- setSocketOption sock Broadcast 1
        connect sock addr
        pure sock
    talk sock = do
        bytes <- send sock $ encode Update
        -- print bytes
        ack <- recv sock 20
        -- print ack
        pure $ either error match $ decode ack
    match Acknowledgement = True
    match _ = False
        -- $ SockAddrInet 3000
        -- $ tupleToHostAddress (255,255,255,255)