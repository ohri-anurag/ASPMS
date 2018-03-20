module Network(
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
import Control.Monad(unless)
import Control.Concurrent(forkIO)
import Data.Serialize
--
import SP6.Data.TimetableRegulation(withHealthyServers)

copyFile :: IO ()
copyFile = pure ()

initTCPServer :: IO ()
initTCPServer = pure ()

serverIPs :: [HostAddress]
serverIPs = map tupleToHostAddress [(192,168,137,1), (192,168,137,61)]

-- TODO Finalise the IP and the port
serverAddrs :: [SockAddr]
serverAddrs = map (SockAddrInet 3000) serverIPs

sendUpdateCommands :: IO ()
sendUpdateCommands = do
    -- Copy the file to a different location, and initialise a tcp server
    copyFile
    initTCPServer
    -- Finally send the update commands
    mapM_ (forkIO . sendUpdateCommand) serverAddrs

sendUpdateCommand :: SockAddr -> IO ()
sendUpdateCommand addr = withSocketsDo $ E.bracket open close talk
    where
    open = do
        sock <- socket AF_INET Stream defaultProtocol
        -- setSocketOption sock Broadcast 1
        setSocketOption sock ReuseAddr 1
        connect sock addr
        pure sock
    talk sock = do
        bytes <- send sock $ encode Update
        -- print bytes
        ack <- recv sock 20
        -- print ack
        unless (either (const False) match $ decode ack) $
            print $ "Invalid acknowledgement received from " ++ (show addr)

    match Acknowledgement = True
    match _ = False
        -- $ SockAddrInet 3000
        -- $ tupleToHostAddress (255,255,255,255)
