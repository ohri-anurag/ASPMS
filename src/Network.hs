{-# LANGUAGE TupleSections #-}
module Network(
    sendUpdateCommands,
    initTCPServer,
    initHeartbeatServer
) where

import Types
import SP6.Data.Command
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

import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Array
import Data.Maybe(catMaybes)

-- VDU Helper Functions
workstationIDToAddr :: WorkstationID -> NetworkID -> String
workstationIDToAddr wrkID nwID = case nwID of
    ATSNetwork1 -> ip1
    ATSNetwork2 -> ip2
    where
        (_,_,_,ip1,ip2,_) = head $ dropWhile notUser asocUserIDToProfile
        notUser (_,id,_,_,_,_) = id /= wrkID

currentHealthyWorkstationsWithNetwork :: Array WorkstationID (MVar (Int, Int)) -> IO [(WorkstationID, NetworkID)]
currentHealthyWorkstationsWithNetwork arrVCommWRK = liftM catMaybes $ forM allElems $ \ wrkID -> do
    comm <- readMVar $ arrVCommWRK <! wrkID
    return $ liftM (wrkID,) $ commToNetwork comm

withHealthyWorkstations :: Array WorkstationID (MVar (Int, Int)) -> (HostName -> IO a) -> IO [a]
withHealthyWorkstations arrVCommSRV process = do
    workstationIDs <- currentHealthyWorkstationsWithNetwork arrVCommSRV
    mapM (process . uncurry workstationIDToAddr) workstationIDs

svFork :: IO () -> IO ()
svFork = withSocketsDo . void . forkIO

-- This function takes a bytestring IORef and returns the same bytestring
-- whenever it receives a request.
-- The same IORef is updated when the user clicks on Apply Changes.
initTCPServer :: IORef B.ByteString -> IO ()
initTCPServer accountBytesRef = svFork $ do
    -- TODO: Change this port
    bracket (openSockTCPServer "port") close $ \sock -> forever $ do
        (conn, peer) <- accept sock
        forkIO $ sendAccountData conn
    where
        sendAccountData conn = void $ do
            accountBytes <- readIORef accountBytesRef
            send conn accountBytes

-- TODO: Change this to use addTimer
initHeartbeatServer :: IO ()
initHeartbeatServer = svFork $ bracket open close $ \sock -> forever $ do
    -- Broadcast Heartbeat for Network 1
    -- TODO: Change the address and port
    sendTo sock (B.pack heartbeatData) $ SockAddrInet heartbeatClientPort $ tupleToHostAddress (127,0,1,1)
    -- Broadcast Heartbeat for Network 2
    -- TODO: Change the address and port
    sendTo sock (B.pack heartbeatData) $ SockAddrInet heartbeatClientPort $ tupleToHostAddress (127,0,2,1)

    -- Wait 500ms before sending next heartbeat
    threadDelay 500000
    where
        open = do
            sock <- socket AF_INET Datagram defaultProtocol
            setSocketOption sock Broadcast 1
            pure sock


sendUpdateCommands :: IO ()
sendUpdateCommands = svFork $ do
    handle <- newMVar stdout
    -- Get the health status for servers
    (arrServerStatus, _) <- initReceiverServerStatus handle False []
    withHealthyServers arrServerStatus sendUpdateCommandToHost
    -- Get the health status for workstations
    (arrWorkstationStatus, _) <- initReceiverWorkstationStatus handle False []
    void $ withHealthyWorkstations arrWorkstationStatus sendUpdateCommandToHost

sendUpdateCommandToHost :: HostName -> IO ()
sendUpdateCommandToHost host = withSocketsDo $ do
    -- TODO: Insert valid port number here
    bracket (openSockTCPClient host "port") close $ \sock -> void
        $ send sock
        $ LB.toStrict
        $ J.encode UpdateRequestCommand
