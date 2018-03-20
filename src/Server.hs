-- Code to be run on VDU app, to decode the update command
module Server where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket hiding (recv, recvFrom, send)
import Network.Socket.ByteString (recv, sendAll, recvFrom, send)
import Foreign.C.Types
import Types
import Data.Serialize as S

sendBroadcast :: IO ()
sendBroadcast = withSocketsDo $ do
    E.bracket open close loop
  where
    open = do
        sock <- socket AF_INET Stream defaultProtocol
        -- print $ addrFamily addr
        -- print $ addrSocketType addr
        -- print $ addrProtocol addr
        print "Server"
        setSocketOption sock ReuseAddr 1
        -- setSocketOption sock Broadcast 1
        bind sock $ SockAddrInet 3000 $ tupleToHostAddress (127,0,0,1)
        listen sock 10
        return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        -- TODO Change the number of bytes to receive
        str <- recv conn 14
        print str
        print $ either error match $ decode str
        -- Send ACK
        bytes <- send conn $ encode Acknowledgement
        print bytes

    match Update = True
    match _ = False
    -- talk conn = do
    --     msg <- recv conn 1024
    --     unless (S.null msg) $ do
    --       sendAll conn msg
    --       talk conn