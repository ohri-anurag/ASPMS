{-# LANGUAGE TupleSections #-}
module Utility where

import Network.Socket
import Control.Concurrent
import Data.Array
import Control.Monad(unless, forever, void, liftM, forM)
import Data.Maybe(catMaybes)

import SP6.Data.ID
import SP6.Data.TimetableRegulation(withHealthyServers, commToNetwork)
import SP6.Data.Common((<!), allElems)

-- VDU Helper Functions
-- TODO: Update this to factor empty lists
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
withHealthyWorkstations arrVCommWRK process = do
    workstationIDs <- currentHealthyWorkstationsWithNetwork arrVCommWRK
    mapM (process . uncurry workstationIDToAddr) workstationIDs
