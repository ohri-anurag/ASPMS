{-# LANGUAGE TupleSections #-}
module Utility(
    workstationIDToAddr,
    currentHealthyWorkstationsWithNetwork,
    withHealthyWorkstations,
    accountFilePath,
    accountFileCopy,
    debugHeartBeat,
    debugMain,
    debugTCP,
    credentialsPath
) where

import Network.Socket
import Control.Concurrent
import Data.Array
import Control.Monad(unless, forever, void, liftM, forM)
import Data.Maybe(catMaybes, mapMaybe, fromJust)
import Data.List(find)

import SP6.Data.ID
import SP6.Data.TimetableRegulation(withHealthyServers, commToNetwork)
import SP6.Data.Common((<!), allElems)

-- VDU Helper Functions
workstationIDToAddr :: WorkstationID -> NetworkID -> String
workstationIDToAddr wrkID nwID = case nwID of
    ATSNetwork1 -> ip1
    ATSNetwork2 -> ip2
    where
        -- No harm in doing fromJust, since we have already filtered workstations in allWorkstations
        (ip1,ip2) = fromJust $ lookup wrkID allWorkstations

-- Remove workstations that are not connected to the network.
allWorkstations :: [(WorkstationID, (String, String))]
allWorkstations = map parse $ mapMaybe (\wrkID -> find (isUser wrkID) asocUserIDToProfile) (allElems :: [WorkstationID])
    where
        isUser wrkID (_,id,_,_,_,_) = id == wrkID
        parse (_,id,_,ip1,ip2,_) = (id,(ip1,ip2))


currentHealthyWorkstationsWithNetwork :: Array WorkstationID (MVar (Int, Int)) -> IO [(WorkstationID, NetworkID)]
currentHealthyWorkstationsWithNetwork arrVCommWRK = liftM catMaybes $ forM allWorkstations $ \ (wrkID,_) -> do
    comm <- readMVar $ arrVCommWRK <! wrkID
    return $ liftM (wrkID,) $ commToNetwork comm

withHealthyWorkstations :: Array WorkstationID (MVar (Int, Int)) -> (HostName -> IO a) -> IO [a]
withHealthyWorkstations arrVCommWRK process = do
    workstationIDs <- currentHealthyWorkstationsWithNetwork arrVCommWRK
    mapM (process . uncurry workstationIDToAddr) workstationIDs

-- Common variables
accountFilePath :: FilePath
accountFilePath = "AccountData"

accountFileCopy :: FilePath
accountFileCopy = "AccountDataCopy"

credentialsPath :: FilePath
credentialsPath = "Credentials"

-- Debugging Functions
debugTCP :: String -> IO ()
debugTCP = putStrLn . (++) "[TCP] "

debugMain :: String -> IO ()
debugMain = putStrLn . (++) "[Main] "

debugHeartBeat :: String -> IO ()
debugHeartBeat = putStrLn . (++) "[HeartBeat] "
-- debug str = do
--     stRef <- newMVar stdout
--     withMVar stRef $ \ h -> do
--         hPutStrLn h str
--         hFlush h
