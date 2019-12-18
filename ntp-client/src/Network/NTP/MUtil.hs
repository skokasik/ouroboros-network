{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE LambdaCase    #-}

module Network.NTP.MUtil
where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, concurrently_, race, waitAnyCancel, withAsync)
import           Control.Concurrent.STM (atomically, check)
import           Control.Concurrent.STM.TBQueue
import           Control.Exception (Exception, IOException, catch, throw)
import           System.IO.Error (tryIOError)
import           Control.Monad (forever, void)
import           Control.Tracer
import           Data.Bifunctor (Bifunctor (..))
import           Data.Binary (decodeOrFail, encode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (traverse_)
import           Data.List (find)
import           Data.Semigroup (First (..), Last (..), Option (..),
                     Semigroup (..))
import           Data.These (These (..))
import           Network.Socket (AddrInfo,
                     AddrInfoFlag (AI_ADDRCONFIG, AI_PASSIVE),
                     Family (AF_INET, AF_INET6), PortNumber, SockAddr (..),
                     Socket, SocketOption (ReuseAddr), SocketType (Datagram),
                     addrAddress, addrFamily, addrFlags, addrSocketType)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString (recvFrom, sendTo)

import           Network.NTP.Packet (NtpPacket, mkNtpPacket, ntpPacketSize)
-- import           Network.NTP.Packet (NtpOffset (..) , NtpPacket (..), clockOffset,  , Microsecond)
import           Network.NTP.Trace (NtpTrace (..))

createAndBindSock
    :: Tracer IO NtpTrace
    -> AddrInfo
    -> IO Socket
createAndBindSock tracer addr = do
    sock <- Socket.socket (addrFamily addr) Datagram Socket.defaultProtocol
    Socket.setSocketOption sock ReuseAddr 1
    Socket.bind sock (addrAddress addr)
    traceWith tracer $ NtpTraceSocketCreated (show $ addrFamily addr) (show $ addrAddress addr)
    return sock

udpLocalAddresses :: IO [AddrInfo]
udpLocalAddresses = do
    let hints = Socket.defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Datagram }
#if MIN_VERSION_network(2,8,0)
        port = Socket.defaultPort
#else
        port = Socket.aNY_PORT
#endif
    --                 Hints        Host    Service
    Socket.getAddrInfo (Just hints) Nothing (Just $ show port)

resolveHost :: String -> IO [AddrInfo]
resolveHost host = Socket.getAddrInfo (Just hints) (Just host) Nothing
  where
    hints = Socket.defaultHints
            { addrSocketType = Datagram
            , addrFlags = [AI_ADDRCONFIG]  -- since we use @AF_INET@ family
            }

isV4Addr :: AddrInfo -> Bool
isV4Addr addr = addrFamily addr == AF_INET

firstIPv4 :: [AddrInfo] -> AddrInfo
firstIPv4 l = case find isV4Addr l of
    Nothing -> error "no IPv4 addr found"
    Just addr -> addr


ntpPort :: PortNumber
ntpPort = 123

replacePort :: PortNumber -> SockAddr ->  SockAddr
replacePort port (SockAddrInet  _ host)            = SockAddrInet  port host
replacePort port (SockAddrInet6 _ flow host scope) = SockAddrInet6 port flow host scope
replacePort _    sockAddr                          = sockAddr



sendPacket
    :: Socket
    -> NtpPacket
    -> SockAddr
    -> IO ()
sendPacket sock packet addr
    = void $ Socket.ByteString.sendTo sock (LBS.toStrict $ encode packet) addr

receivePacket :: Tracer IO NtpTrace -> Socket -> IO ()
receivePacket tracer sock = do
    (bs, _) <- Socket.ByteString.recvFrom sock ntpPacketSize
    case decodeOrFail $ LBS.fromStrict bs of
        Left  (_, _, err) -> traceWith tracer $ NtpTraceReceiveLoopDecodeError err
        Right (_, _, packet) -> handleNtpPacket packet
  where
    handleNtpPacket
        :: NtpPacket
        -> IO ()
    handleNtpPacket packet = traceWith tracer NtpTraceReceiveLoopPacketReceived

testQuery :: IO ()
testQuery = do  
  let
    tracer :: Tracer IO NtpTrace
    tracer = contramapM (return . show) stdoutTracer
  socket <- (firstIPv4 <$> udpLocalAddresses) >>= createAndBindSock tracer
  dest <- firstIPv4 <$> resolveHost "0.pool.ntp.org"
  print dest
  
  (getLine>>= error "User Enter")
    `concurrently_` (forever $ receivePacket tracer socket)
    `concurrently_` (sendLoop socket dest)

  where
    sendLoop socket dest = forever $ do
        pack <- mkNtpPacket
        sendPacket socket pack $ (replacePort ntpPort $ Socket.addrAddress dest)
        print "send"
        threadDelay 10000000


testClient :: IO ()
testClient = do  
  let
    tracer :: Tracer IO NtpTrace
    tracer = contramapM (return . show) stdoutTracer
  client tracer

client tracer = withResources tracer $ \(socket, addresses, inQueue) -> do
    error <- race (socketReaderThread tracer inQueue socket)
                  (runQueryLoop tracer inQueue socket addresses)
    --- rethrow /log errors
    return ()


socketReaderThread :: Tracer IO NtpTrace -> TBQueue NtpPacket -> Socket -> IO (Either IOError ())
socketReaderThread tracer inQueue socket = tryIOError $ forever $ do
    (bs, _) <- Socket.ByteString.recvFrom socket ntpPacketSize
    case decodeOrFail $ LBS.fromStrict bs of
        Left  (_, _, err) -> traceWith tracer $ NtpTraceSocketReaderDecodeError err
        Right (_, _, packet) -> do
          traceWith tracer NtpTraceReceiveLoopPacketReceived
          atomically $ writeTBQueue inQueue packet

runQueryLoop :: Tracer IO NtpTrace -> TBQueue NtpPacket -> Socket -> AddrInfo -> IO (Either IOError ())
runQueryLoop tracer inQueue socket addresses = tryIOError $ forever $ do
    traceWith tracer NtpTraceClientStartQuery
    void $ atomically $ flushTBQueue inQueue
    (_id, replies) <- withAsync (send tracer socket addresses) $ \_sender -> do
        t1 <- async $ timeout inQueue
        t2 <- async $ enoughtReplies inQueue 3
        waitAnyCancel [t1, t2]
    print replies
    threadDelay 20000000
    where
        timeout q = do
            threadDelay 5000000
            traceWith tracer NtpTraceClientWaitingForRepliesTimeout
            atomically $ flushTBQueue q

        enoughtReplies q n = atomically $ do
            l <- lengthTBQueue q
            check $ l >= n
            flushTBQueue q

        send tracer sock addr = do
            p <- mkNtpPacket
            void $ Socket.ByteString.sendTo sock (LBS.toStrict $ encode p) (replacePort ntpPort $ Socket.addrAddress addr)
          
-- todo : use bracket here
withResources :: Tracer IO NtpTrace -> ((Socket, AddrInfo, TBQueue NtpPacket) -> IO ()) -> IO ()
withResources tracer action = do
    socket <- (firstIPv4 <$> udpLocalAddresses) >>= createAndBindSock tracer
    dest <- firstIPv4 <$> resolveHost "0.pool.ntp.org"
    inQueue <- atomically $ newTBQueue 100 -- ???
    action (socket, dest, inQueue)
