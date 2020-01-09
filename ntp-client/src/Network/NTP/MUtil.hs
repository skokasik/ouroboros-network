{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}

module Network.NTP.MUtil
where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async -- (Async, async, link, race, race_, waitAnyCancel, withAsync)
import           Control.Concurrent.STM (STM, atomically, check, retry)
import           Control.Concurrent.STM.TVar
import           Control.Exception (bracket)
import           System.IO.Error (tryIOError)
import           System.IO.Error (userError, ioError) -- testing
import           Control.Monad (forever, void, forM, forM_)
import           Control.Tracer
import           Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.List (find)
import           Network.Socket (AddrInfo,
                     AddrInfoFlag (AI_ADDRCONFIG, AI_PASSIVE),
                     Family (AF_INET, AF_INET6), PortNumber, SockAddr (..),
                     Socket, SocketOption (ReuseAddr), SocketType (Datagram),
                     addrAddress, addrFamily, addrFlags, addrSocketType)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString (recvFrom, sendTo)

import           Network.NTP.Packet (NtpPacket, mkNtpPacket, ntpPacketSize, Microsecond, NtpOffset (..), getCurrentTime, clockOffsetPure)
-- import           Network.NTP.Packet ( , NtpPacket (..), clockOffset,
import           Network.NTP.Trace (NtpTrace (..))


main :: IO ()
main = testClient

data NtpClientSettings = NtpClientSettings
    { ntpServers         :: [String]
      -- ^ list of servers addresses
    , ntpResponseTimeout :: Microsecond
      -- ^ delay between making requests and response collection
    , ntpPollDelay       :: Microsecond
      -- ^ how long to wait between to send requests to the servers
    , ntpReportPolicy  :: ReportPolicy
    }

data NtpClient = NtpClient
    { -- | Query the current NTP status.
      ntpGetStatus        :: STM NtpStatus
      -- | Bypass all internal threadDelays and trigger a new NTP query.
    , ntpTriggerUpdate    :: IO ()
    , ntpThread           :: Async ()
    }

data NtpStatus =
      -- | The difference between NTP time and local system time
      NtpDrift NtpOffset
      -- | NTP client has send requests to the servers
    | NtpSyncPending
      -- | NTP is not available: the client has not received any respond within
      -- `ntpResponseTimeout` or NTP was not configured.
    | NtpSyncUnavailable deriving (Eq, Show)

type ReportPolicy = [ReceivedPacket] -> Summary

data Summary = Report NtpOffset | Wait
    deriving (Eq, Show)

data ReceivedPacket = ReceivedPacket
    { receivedPacket    :: NtpPacket
    , receivedLocalTime :: Microsecond
    , receivedOffset    :: NtpOffset
    } deriving (Eq, Show)

minimumOfThree :: ReportPolicy
minimumOfThree l
    = if length l >= 3 then Report $ minimum $ map receivedOffset l
         else Wait

-- | Setup a NtpClient and run a computation that uses that client.
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
              , ntpThread = tid
              }
        link tid         -- an error in the ntp-client kills the appliction !
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

firstIPv4 :: String -> [AddrInfo] -> IO AddrInfo
firstIPv4 name l = case find isV4Addr l of
    Nothing -> ioError $ userError $ "host :" ++ name ++ "IPv4 addr not found"
    Just addr -> return addr

setNtpPort :: SockAddr ->  SockAddr
setNtpPort addr = case addr of
    (SockAddrInet  _ host)            -> SockAddrInet  ntpPort host
    (SockAddrInet6 _ flow host scope) -> SockAddrInet6 ntpPort flow host scope
    sockAddr                   -> sockAddr
  where
    ntpPort :: PortNumber
    ntpPort = 123

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

socketReaderThread :: Tracer IO NtpTrace -> TVar [ReceivedPacket] -> Socket -> IO (Either IOError ())
socketReaderThread tracer inQueue socket = tryIOError $ forever $ do
    (bs, _) <- Socket.ByteString.recvFrom socket ntpPacketSize
    t <- getCurrentTime
    case decodeOrFail $ LBS.fromStrict bs of
        Left  (_, _, err) -> traceWith tracer $ NtpTraceSocketReaderDecodeError err
        Right (_, _, packet) -> do
            traceWith tracer NtpTraceReceiveLoopPacketReceived
            let received = ReceivedPacket packet t (clockOffsetPure packet t)
            atomically $ modifyTVar' inQueue ((:) received)

threadDelayInterruptible :: TVar NtpStatus -> Int -> IO ()
threadDelayInterruptible tvar t
    = race_
       ( threadDelay t )
       ( atomically $ do
           s <- readTVar tvar
           check $ s == NtpSyncPending
       )

data QueryOutcome = Timeout | Result NtpOffset

runQueryLoop ::
       Tracer IO NtpTrace
    -> NtpClientSettings
    -> TVar NtpStatus
    -> TVar [ReceivedPacket]
    -> (Socket, [AddrInfo])
    -> IO (Either IOError ())
runQueryLoop tracer ntpSettings ntpStatus inQueue servers = tryIOError $ forever $ do
    traceWith tracer NtpTraceClientStartQuery
    void $ atomically $ writeTVar inQueue []
    (_id, outcome) <- runThreads
    case outcome of
        Timeout -> do
            traceWith tracer NtpTraceUpdateStatusQueryFailed
            atomically $ writeTVar ntpStatus NtpSyncUnavailable
        Result offset -> do
             traceWith tracer $ NtpTraceUpdateStatusClockOffset $ getNtpOffset offset
             atomically $ writeTVar ntpStatus $ NtpDrift offset
             
    traceWith tracer NtpTraceClientSleeping
    threadDelayInterruptible ntpStatus $ fromIntegral $ ntpPollDelay ntpSettings
    where
        runThreads
           = withAsync (send servers) $ \sender ->
               withAsync timeout        $ \delay ->
                 withAsync checkReplies   $ \revc ->
                    waitAnyCancel [sender, delay, revc]

        timeout = do
            threadDelay $ fromIntegral $ ntpResponseTimeout ntpSettings
            traceWith tracer NtpTraceClientWaitingForRepliesTimeout
            return Timeout

        checkReplies = do
            r <- atomically $ do
                l <- readTVar inQueue
                case (ntpReportPolicy ntpSettings) l of
                     Wait -> retry
                     Report r -> return r
            return $ Result r

        send (sock, addrs) = do
            forM_ addrs $ \addr -> do
                p <- mkNtpPacket
                void $ Socket.ByteString.sendTo sock (LBS.toStrict $ encode p) (setNtpPort $ Socket.addrAddress addr)
                traceWith tracer NtpTracePacketSent
            forever $ threadDelay 1_000_000
            
testClient :: IO ()
testClient = withNtpClient (contramapM (return . show) stdoutTracer) settings runApplication
  where
    runApplication ntpClient = race_ getLine $ forever $ do
        status <- atomically $ ntpGetStatus ntpClient
        traceWith stdoutTracer $ show ("main"::String, status)
        threadDelay 10_000_000
        ntpTriggerUpdate ntpClient

    settings :: NtpClientSettings
    settings = NtpClientSettings
        { ntpServers = ["0.de.pool.ntp.org","0.europe.pool.ntp.org","0.pool.ntp.org","1.pool.ntp.org","2.pool.ntp.org","3.pool.ntp.org"]
        , ntpResponseTimeout = fromInteger 5_000_000
        , ntpPollDelay       = fromInteger 300_000_000
        , ntpReportPolicy    = minimumOfThree
        }

-- TODO: maybe reset the delaytime if the oneshotClient did one sucessful query
ntpClientThread ::
       Tracer IO NtpTrace
    -> (NtpClientSettings, TVar NtpStatus)
    -> IO ()
ntpClientThread tracer args@(_, ntpStatus) = forM_ restartDelay $ \t -> do
    traceWith tracer $ NtpTraceRestartDelay t
    threadDelayInterruptible ntpStatus $ t * 1_000_000
    traceWith tracer NtpTraceRestartingClient
    oneshotClient tracer args
    atomically $ writeTVar ntpStatus NtpSyncUnavailable
    where
      restartDelay :: [Int]
      restartDelay = [0, 5, 10, 20, 60, 180, 600] ++ repeat 600

oneshotClient ::
       Tracer IO NtpTrace
    -> (NtpClientSettings, TVar NtpStatus)
    -> IO ()
oneshotClient tracer (ntpSettings, ntpStatus)
  = (tryIOError $ bracket acquire release action) >>= \case
      Right () -> return ()
      Left err -> traceWith tracer $ NtpOneshotClientIOError err
  where
    action :: (Socket, [AddrInfo], TVar [ReceivedPacket]) -> IO ()
    action (socket, addresses, inQueue) = do
        err <- race (socketReaderThread tracer inQueue socket)
                  (runQueryLoop tracer ntpSettings ntpStatus inQueue (socket, addresses) )
        case err of
            (Right (Left e)) -> traceWith tracer $ NtpTraceQueryLoopIOException e
            (Left  (Left e)) -> traceWith tracer $ NtpTraceSocketReaderIOException e
            _ -> error "unreachable"

    acquire :: IO (Socket, [AddrInfo], TVar [ReceivedPacket])
    acquire = do
        dest <- forM (ntpServers ntpSettings) $ \server -> resolveHost server >>= firstIPv4 server
        socket <- udpLocalAddresses >>= firstIPv4 "localhost" >>= createAndBindSock tracer
        inQueue <- atomically $ newTVar []
        return (socket, dest, inQueue)

    release :: (Socket, [AddrInfo], TVar [ReceivedPacket]) -> IO ()
    release (sock, _, _) = do
        Socket.close sock
        traceWith tracer $ NtpTraceSocketClosed
