{-# LANGUAGE TupleSections #-}
module Network(
    sendUpdateCommands,
    initTCPServer
) where

import Types
import SP6.Data.Command
import SP6.Data.ID
import SP6.CommonIO
import SP6.Data.IOConfig
import SP6.Data.TimetableRegulation(withHealthyServers, commToNetwork)
import SP6.Data.Common((<!), allElems)

import System.IO(stdout)

import Control.Exception
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)

import Control.Monad(unless, forever, void, liftM, forM)
import Control.Concurrent(forkIO, MVar, newMVar, readMVar)

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

initTCPServer :: IO ()
-- TODO: Change this port
initTCPServer = void $ forkIO $ bracket (openSockTCPServer "port") close $ \sock -> forever $ do
    bytes <- B.readFile "data/AccountData"
    bytes `seq` do
        (conn, peer) <- accept sock
        forkIO $ void $ send conn bytes

sendUpdateCommands :: IO ()
sendUpdateCommands = void $ forkIO $ do
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
