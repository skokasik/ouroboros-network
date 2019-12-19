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
import           Control.Concurrent.Async (Async, async, concurrently_, race, race_, waitAnyCancel, withAsync)
import           Control.Concurrent.STM (STM, atomically, check)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import           Control.Exception (Exception, IOException, catch, throw)
import           System.IO.Error (tryIOError)
import           Control.Monad (forever, void, forM, forM_)
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

import           Network.NTP.Packet (NtpPacket, mkNtpPacket, ntpPacketSize, Microsecond, NtpOffset (..))
-- import           Network.NTP.Packet ( , NtpPacket (..), clockOffset,
import           Network.NTP.Trace (NtpTrace (..))


data NtpClientSettings = NtpClientSettings
    { ntpServers         :: [String]
      -- ^ list of servers addresses
    , ntpResponseTimeout :: Microsecond
      -- ^ delay between making requests and response collection
    , ntpPollDelay       :: Microsecond
      -- ^ how long to wait between to send requests to the servers
    }

data NtpClient = NtpClient
    { -- | Query the current NTP status.
      ntpGetStatus        :: STM NtpStatus
      -- | Bypass all internal threadDelays and trigger a new NTP query.
    , ntpTriggerUpdate    :: IO ()
    , ntpClient           :: Async ()
    }

data NtpStatus =
      -- | The difference between NTP time and local system time
      NtpDrift NtpOffset
      -- | NTP client has send requests to the servers
    | NtpSyncPending
      -- | NTP is not available: the client has not received any respond within
      -- `ntpResponseTimeout` or NTP was not configured.
    | NtpSyncUnavailable deriving (Eq, Show)

-- | Setup a NtpClient and run a computation that uses that client.
-- Todo : proper bracket-style tear-down of the NTP client.
withNtpClient :: Tracer IO NtpTrace -> NtpClientSettings -> (NtpClient -> IO a) -> IO a
withNtpClient tracer ntpSettings action = do
    traceWith tracer NtpTraceStartNtpClient
    ntpStatus <- newTVarIO NtpSyncPending
    withAsync (ntpClientThread tracer (ntpSettings, ntpStatus)) $ \tid -> do
        let client = NtpClient
              { ntpGetStatus = readTVar ntpStatus
              , ntpTriggerUpdate = do
                   traceWith tracer NtpTraceClientActNow
                   atomically $ writeTVar ntpStatus NtpSyncPending
              , ntpClient = tid
              }
        action client

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

setNtpPort :: SockAddr ->  SockAddr
setNtpPort addr = case addr of
    (SockAddrInet  _ host)            -> SockAddrInet  ntpPort host
    (SockAddrInet6 _ flow host scope) -> SockAddrInet6 ntpPort flow host scope
    sockAddr                   -> sockAddr
  where
    ntpPort :: PortNumber
    ntpPort = 123

socketReaderThread :: Tracer IO NtpTrace -> TBQueue NtpPacket -> Socket -> IO (Either IOError ())
socketReaderThread tracer inQueue socket = tryIOError $ forever $ do
    (bs, _) <- Socket.ByteString.recvFrom socket ntpPacketSize
    case decodeOrFail $ LBS.fromStrict bs of
        Left  (_, _, err) -> traceWith tracer $ NtpTraceSocketReaderDecodeError err
        Right (_, _, packet) -> do
          traceWith tracer NtpTraceReceiveLoopPacketReceived
          atomically $ writeTBQueue inQueue packet

data QueryOutcome
    = Timeout [NtpPacket]
    | SuitableReplies [NtpPacket]

runQueryLoop ::
       Tracer IO NtpTrace
    -> NtpClientSettings
    -> TVar NtpStatus
    -> TBQueue NtpPacket
    -> (Socket, [AddrInfo])
    -> IO (Either IOError ())
runQueryLoop tracer ntpSettings ntpStatus inQueue servers = tryIOError $ forever $ do
    traceWith tracer NtpTraceClientStartQuery
    void $ atomically $ flushTBQueue inQueue
    (_id, outcome) <- withAsync (send tracer servers) $ \_sender -> do
        t1 <- async $ timeout inQueue
        t2 <- async $ checkReplies inQueue 3
        waitAnyCancel [t1, t2]
    case outcome of
        Timeout _ -> do
            traceWith tracer NtpTraceUpdateStatusQueryFailed
            atomically $ writeTVar ntpStatus NtpSyncUnavailable
        SuitableReplies l -> do
             traceWith tracer $ NtpTraceUpdateStatusClockOffset 0
             atomically $ writeTVar ntpStatus $ NtpDrift $ minimum [0]
             
    traceWith tracer NtpTraceClientSleeping
    threadDelay $ fromIntegral $ ntpPollDelay ntpSettings
      where
        timeout q = do
            threadDelay $ fromIntegral $ ntpResponseTimeout ntpSettings
            traceWith tracer NtpTraceClientWaitingForRepliesTimeout
            Timeout <$> (atomically $ flushTBQueue q)

        checkReplies q n = do
            r <- atomically $ do
                len <- lengthTBQueue q
                check $ len >= n
                flushTBQueue q
            return $ SuitableReplies r

        send tracer (sock, addrs) = forM_ addrs $ \addr -> do
            p <- mkNtpPacket
            putStrLn "packetSend"
            void $ Socket.ByteString.sendTo sock (LBS.toStrict $ encode p) (setNtpPort $ Socket.addrAddress addr)

testClient :: IO ()
testClient = withNtpClient (contramapM (return . show) stdoutTracer) settings runClient
  where
    runClient ntpClient = race_ getLine $ forever $ do
            status <- atomically $ ntpGetStatus ntpClient
            traceWith stdoutTracer $ show ("main",status)
            threadDelay 3000000

    settings :: NtpClientSettings
    settings = NtpClientSettings
        { ntpServers = ["0.de.pool.ntp.org","0.europe.pool.ntp.org","0.pool.ntp.org","1.pool.ntp.org","2.pool.ntp.org","3.pool.ntp.org"]
        , ntpResponseTimeout = fromInteger 3000000
        , ntpPollDelay       = fromInteger 10000000
        }


ntpClientThread ::
       Tracer IO NtpTrace
    -> (NtpClientSettings, TVar NtpStatus)
    -> IO ()
ntpClientThread = oneshotClient

oneshotClient ::
       Tracer IO NtpTrace
    -> (NtpClientSettings, TVar NtpStatus)
    -> IO ()
oneshotClient tracer (ntpSettings, ntpStatus) = withResources $ \(socket, addresses, inQueue) -> do
    error <- race (socketReaderThread tracer inQueue socket)
                  (runQueryLoop tracer ntpSettings ntpStatus inQueue (socket, addresses) )
    --- rethrow /log errors
    return ()
 where
-- todo : use bracket here
    withResources :: ((Socket, [AddrInfo], TBQueue NtpPacket) -> IO ()) -> IO ()
    withResources action = do
        socket <- (firstIPv4 <$> udpLocalAddresses) >>= createAndBindSock tracer
        dest <- forM (ntpServers ntpSettings) $ \server -> firstIPv4 <$> resolveHost server
        inQueue <- atomically $ newTBQueue 100 -- ???
        action (socket, dest, inQueue)

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
