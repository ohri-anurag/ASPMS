-- Code to be run on VDU app, to decode the update command
module Server where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket hiding (recv, recvFrom)
import Network.Socket.ByteString (recv, sendAll, recvFrom)
import Foreign.C.Types
import Types
import Data.Serialize as S

sendBroadcast :: IO ()
sendBroadcast = withSocketsDo $ do
    addr <- resolve "192.168.137.1" "3000"
    E.bracket (open addr) close loop
  where
    resolve host port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Datagram
              }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        -- print $ addrFamily addr
        -- print $ addrSocketType addr
        -- print $ addrProtocol addr
        print "Server"
        setSocketOption sock ReuseAddr 1
        -- setSocketOption sock Broadcast 1
        bind sock (addrAddress addr)
        -- listen sock 10
        return sock
    loop sock = forever $ do
        (str, peer) <- recvFrom sock 14
        -- (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        print $ decodeCommand str
        -- void $ forkFinally (talk conn) (\_ -> close conn)
    talk conn = do
        msg <- recv conn 1024
        unless (S.null msg) $ do
          sendAll conn msg
          talk conn