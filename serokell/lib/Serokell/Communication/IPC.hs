{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Serokell.Communication.IPC
       ( NodeId (..)
       , Recv
       , Send
       , Conversation (..)
       , SocketDirectory

       , connectToUnixSocket
       , listenUnixSocket
       ) where

import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.QSem   (QSem, newQSem, signalQSem, waitQSem)
import           Control.Exception.Safe    (bracket, bracket_)
import           Control.Monad             (when)
import           Data.Binary               (Binary)
import           Data.ByteString           (ByteString)
import           Data.Foldable             (traverse_)
import           System.Directory          (getPermissions, executable)
import           System.Environment        (lookupEnv, setEnv)
import           System.FilePath           ((<.>), (</>))
import           System.IO.Unsafe          (unsafePerformIO)
import           Text.Read                 (readMaybe)

import qualified Network.Socket            as Net
import qualified Network.Socket.ByteString as NetByte

-- | Type alias for file path, representing directory with sockets.
type SocketDirectory = FilePath

-- | Id of node.
newtype NodeId = NodeId { unNodeId :: Int }
    deriving (Eq, Ord, Binary)

type Send = ByteString -> IO ()
type Recv = IO ByteString

-- | Newtype wrapper for sending action to socket.
data Conversation = Conversation
    { send :: ByteString -> IO ()  -- ^ sends given 'ByteString' to socket
    , recv :: IO ByteString  -- ^ recieves 'ByteString' with length up to 4096 bytes from socket
    }

readEnv :: Read a => String -> IO (Maybe a)
readEnv name = (>>= readMaybe) <$> lookupEnv name

-- blocking QSem for multithreaded sending environment
-- reads env variable SRK_SOCK_CONC, concurrency parameter (capacity of semaphore)
sendSem :: Maybe QSem
sendSem = unsafePerformIO $ readEnv "SRK_SOCK_CONC" >>= traverse newQSem

{-# NOINLINE sendSem #-}

-- delay on outbound send (in milliseconds)
-- reads env variable SRK_SOCK_DELAY
sendDelay :: Maybe Int
sendDelay = unsafePerformIO $ readEnv "SRK_SOCK_DELAY"

{-# NOINLINE sendDelay #-}

withQSem :: QSem -> IO a -> IO a
withQSem qsem = bracket_ (waitQSem qsem) (signalQSem qsem)

sendSock :: Net.Socket -> Send
sendSock = NetByte.sendAll

recvSock :: Net.Socket -> Recv
recvSock conn = NetByte.recv conn 4096

-- Creates socket using provided allocator and handles all exceptions using bracket function.
initializeSocket :: IO Net.Socket -> (Net.Socket -> IO a) -> IO a
initializeSocket before = bracket before Net.close

-- Local variable for storing NodeId of current node
localSocketFilePath :: FilePath
localSocketFilePath = "SOCKET_FILE"

-- | Create new UNIX socket for node 'NodeId', listen to it and use supplied
-- handler to process incoming connections.
listenUnixSocket :: SocketDirectory
                 -> NodeId
                 -> (Conversation -> IO Bool)
                 -> IO ()
listenUnixSocket socketDirectory (NodeId nodeId) handler =
    initializeSocket allocator acceptLoop
  where
    socketPath :: FilePath
    socketPath = socketDirectory </> show nodeId <.> "sock"

    acceptLoop :: Net.Socket -> IO ()
    acceptLoop socket = acceptServerSock >>= handler >>= flip when (acceptLoop socket)
      where
        acceptServerSock :: IO Conversation
        acceptServerSock = do
          setEnv localSocketFilePath socketPath
          (conn, _) <- Net.accept socket
          return $ Conversation (sendSock conn) (recvSock conn)

    allocator :: IO Net.Socket
    allocator = do
        socket <- Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol
        Net.bind socket $ Net.SockAddrUnix socketPath
        Net.listen socket 5
        return socket

-- | Connect and communicate to unix socket for node 'NodeId', handler is supplied.
connectToUnixSocket :: SocketDirectory
                    -> NodeId
                    -> (Conversation -> IO a)
                    -> IO a
connectToUnixSocket socketDirectory (NodeId nodeId) handler =
    initializeSocket allocator (handler . outboundSendRecv)
  where
    socketPath :: FilePath
    socketPath = socketDirectory </> show nodeId <.> "sock"

    outboundSendRecv :: Net.Socket -> Conversation
    outboundSendRecv socket = Conversation outSend (recvSock socket)
      where
        outSend :: Send
        outSend data_ =
            maybe id withQSem sendSem $ do
                mLocalSocketFile <- lookupEnv localSocketFilePath

                executableLocalFile <- case mLocalSocketFile of
                  Nothing -> pure True
                  Just localSocketFile -> executable <$> getPermissions localSocketFile

                permission <- getPermissions socketPath
                if executable permission && executableLocalFile
                  then do
                    traverse_ msDelay sendDelay
                    sendDo data_
                  else Net.close socket

        sendDo :: ByteString -> IO ()
        sendDo = sendSock socket

        msDelay :: Int -> IO ()
        msDelay milliseconds = threadDelay $ milliseconds * 10^(3 :: Int)

    allocator :: IO Net.Socket
    allocator = do
        socket <- Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol
        Net.connect socket $ Net.SockAddrUnix socketPath
        return socket
