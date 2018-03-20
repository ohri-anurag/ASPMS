module Network(
    sendUpdateCommands
) where

import Types

import Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv, sendTo, send)
import Network.Socket.ByteString (recv, sendTo, send)
import Control.Monad(unless, forever, void)
import Control.Concurrent(forkIO)
import Data.Serialize
import qualified Data.ByteString as B
--
import SP6.Data.TimetableRegulation(withHealthyServers)

initTCPServer :: IO ()
initTCPServer = void $ forkIO $ bracket open close talk
    where
        open = do
            sock <- socket AF_INET Stream defaultProtocol
            setSocketOption sock ReuseAddr 1
            -- TODO Decide this port and IP
            bind sock $ SockAddrInet 3000 $ tupleToHostAddress (127,0,0,1)
            listen sock 10
            pure sock
        talk sock = forever $ do
            bytes <- B.readFile "data/AccountData"
            bytes `seq` do
                (conn, peer) <- accept sock
                forkIO $ void $ send conn bytes

serverIPs :: [HostAddress]
serverIPs = map tupleToHostAddress [(192,168,137,1), (192,168,137,61)]

-- TODO Finalise the IP and the port
serverAddrs :: [SockAddr]
serverAddrs = map (SockAddrInet 3000) serverIPs

sendUpdateCommands :: IO ()
sendUpdateCommands = do
    -- Initialize a tcp server
    initTCPServer
    -- And send the update commands
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
