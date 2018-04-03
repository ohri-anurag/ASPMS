module Network(
    sendUpdateCommands,
    initTCPServer,
    initHeartbeatServer,
    withSocketsDo
) where

import Utility

import SP6.Data.Account
import SP6.Data.ID
import SP6.CommonIO
import SP6.Data.IOConfig
import SP6.Data.TimetableRegulation(withHealthyServers)

import Data.IORef
import Control.Exception
import Network.Socket hiding (send, sendTo)
import Network.Socket.ByteString (send, sendTo)

import Control.Monad(void, forever)
import Control.Concurrent(MVar, forkIO)

import Data.Serialize
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Array

svFork :: IO () -> IO ()
svFork = withSocketsDo . void . forkIO

-- This function takes a bytestring IORef and returns the same bytestring
-- whenever it receives a request.
-- The same IORef is updated when the user clicks on Apply Changes.
initTCPServer :: IORef B.ByteString -> IO ()
initTCPServer accountBytesRef = svFork $ do
    debugTCP "Creating socket and binding..."
    bracket (openSockTCPServer portNumberAccountServer) close $ \sock -> forever $ do
        debugTCP "Opened socket, waiting for connections..."
        (conn, peer) <- accept sock
        debugTCP $ "Accepted connection from " ++ show peer
        forkIO $ sendAccountData conn
    where
        sendAccountData conn = void $ do
            accountBytes <- readIORef accountBytesRef
            _ <- send conn accountBytes
            close conn

initHeartbeatServer :: IO ()
initHeartbeatServer = svFork $ bracket open close $ \sock -> do
    let hints = defaultHints {addrSocketType = Datagram}
    addr1:_ <- getAddrInfo (Just hints) (Just (networkToAddr ATSNetwork1)) (Just portNumberASPMHeartBeat)
    addr2:_ <- getAddrInfo (Just hints) (Just (networkToAddr ATSNetwork2)) (Just portNumberASPMHeartBeat)
    addTimer 500000 $ void $ do
        -- Broadcast Heartbeat for Network 1
        _ <- sendTo sock (encode APSMHeartBeat) $ addrAddress addr1
        -- Broadcast Heartbeat for Network 2
        sendTo sock (encode APSMHeartBeat) $ addrAddress addr2
    where
        open = do
            sock <- socket AF_INET Datagram defaultProtocol
            setSocketOption sock Broadcast 1
            pure sock

-- TODO: Every 500 ms decrement the server and workstation health.
-- Confirm the time interval(500 ms).
sendUpdateCommands :: Array ServerID (MVar (Int, Int)) -> Array WorkstationID (MVar (Int, Int)) -> IO ()
sendUpdateCommands arrServerStatus arrWorkstationStatus = svFork $ do
    debugMain "Sending update command to servers..."
    _ <- withHealthyServers arrServerStatus sendUpdateCommandToHost
    debugMain "Sending update command to workstations..."
    _ <- withHealthyWorkstations arrWorkstationStatus sendUpdateCommandToHost
    pure ()

sendUpdateCommandToHost :: HostName -> IO ()
sendUpdateCommandToHost host = svFork $ do
    debugMain $ "Sending update command to " ++ host
    bracket (openSockTCPClient host updateRequestCommandPortNumber) close $ \sock -> void $ do
        _ <- send sock $ LB.toStrict $ J.encode UpdateRequestCommand
        close sock
