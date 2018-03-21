module Network(
    sendUpdateCommands,
    initTCPServer,
    initHeartbeatServer,
    withSocketsDo
) where

import Types
import Utility

import SP6.Data.Account
import SP6.Data.ID
import SP6.CommonIO
import SP6.Data.IOConfig
import SP6.Data.TimetableRegulation(withHealthyServers, commToNetwork)
import SP6.Data.Common((<!), allElems)

import System.IO(stdout)
import Data.IORef
import Control.Exception
import Network.Socket hiding (send, sendTo)
import Network.Socket.ByteString (send, sendTo)

import Control.Monad(unless, forever, void, liftM, forM)
import Control.Concurrent(forkIO, MVar, newMVar, readMVar, threadDelay)

import Data.Serialize
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Array
import Data.Maybe(catMaybes)

svFork :: IO () -> IO ()
svFork = withSocketsDo . void . forkIO

-- This function takes a bytestring IORef and returns the same bytestring
-- whenever it receives a request.
-- The same IORef is updated when the user clicks on Apply Changes.
initTCPServer :: IORef B.ByteString -> IO ()
initTCPServer accountBytesRef = svFork $ do
    printDebug "Creating socket and binding..."
    bracket (openSockTCPServer portNumberAccountServer) close $ \sock -> forever $ do
        printDebug $ "Opened socket, waiting for connections..."
        (conn, peer) <- accept sock
        printDebug $ "Accepted connection from " ++ (show peer)
        forkIO $ sendAccountData conn
    where
        sendAccountData conn = void $ do
            accountBytes <- readIORef accountBytesRef
            send conn accountBytes
            close conn

initHeartbeatServer :: IO ()
initHeartbeatServer = svFork $ bracket open close $ \sock -> do
    let hints = defaultHints {addrSocketType = Datagram}
    addr1:_ <- getAddrInfo (Just hints) (Just (networkToAddr ATSNetwork1)) (Just portNumberASPMHeartBeat)
    addr2:_ <- getAddrInfo (Just hints) (Just (networkToAddr ATSNetwork2)) (Just portNumberASPMHeartBeat)
    addTimer 500000 $ void $ do
        -- Broadcast Heartbeat for Network 1
        sendTo sock (encode APSMHeartBeat) $ addrAddress addr1
        -- Broadcast Heartbeat for Network 2
        sendTo sock (encode APSMHeartBeat) $ addrAddress addr2
    where
        open = do
            sock <- socket AF_INET Datagram defaultProtocol
            setSocketOption sock Broadcast 1
            pure sock


sendUpdateCommands :: Array ServerID (MVar (Int, Int)) -> Array WorkstationID (MVar (Int, Int)) -> IO ()
sendUpdateCommands arrServerStatus arrWorkstationStatus = svFork $ do
    health <- readMVar $ arrWorkstationStatus <! WS801
    print health
    withHealthyServers arrServerStatus sendUpdateCommandToHost
    withHealthyWorkstations arrWorkstationStatus sendUpdateCommandToHost
    -- sendUpdateCommandToHost "172.21.102.1"
    pure ()

sendUpdateCommandToHost :: HostName -> IO ()
sendUpdateCommandToHost host = svFork $ do
    putStrLn $ "Sending update command to " ++ host
    bracket (openSockTCPClient host updateRequestCommandPortNumber) close $ \sock -> void $ do
        send sock $ LB.toStrict $ J.encode UpdateRequestCommand
        close sock
