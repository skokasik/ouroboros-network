{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}

module Network.NTP.Client
where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM (STM, atomically, check, retry)
import           Control.Concurrent.STM.TVar
import           Control.Exception (bracket)
import           System.IO.Error (tryIOError, userError, ioError)
import           Control.Monad (forever, void, forM, forM_)
import           Control.Tracer
import           Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.List (find)
import           Data.Maybe
import           Data.These
import           Network.Socket ( AddrInfo,
                     AddrInfoFlag (AI_ADDRCONFIG, AI_PASSIVE),
                     Family (AF_INET, AF_INET6), PortNumber, SockAddr (..),
                     Socket, SocketOption (ReuseAddr), SocketType (Datagram),
                     addrAddress, addrFamily, addrFlags, addrSocketType)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString (recvFrom, sendManyTo)
import           Network.NTP.Packet (NtpPacket, mkNtpPacket, ntpPacketSize, Microsecond,
                     NtpOffset (..), getCurrentTime, clockOffsetPure)
import           Network.NTP.Trace (NtpTrace (..))


main :: IO ()
main = testClient

data NtpClientSettings = NtpClientSettings
    { ntpServers         :: [String]
      -- ^ List of servers addresses.
    , ntpResponseTimeout :: Microsecond
      -- ^ Timeout between sending NTP requests and response collection.
    , ntpPollDelay       :: Microsecond
      -- ^ How long to wait between two rounds of requests.
    , ntpReportPolicy    :: ReportPolicy
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

data Summary = Report !NtpOffset | Wait
    deriving (Eq, Show)

data ReceivedPacket = ReceivedPacket
    { receivedPacket    :: !NtpPacket
    , receivedLocalTime :: !Microsecond
    , receivedOffset    :: !NtpOffset
    } deriving (Eq, Show)

-- | Wait for at least three replies and report the minimum of the reported offsets.
minimumOfThree :: ReportPolicy
minimumOfThree l
    = if length l >= 3 then Report $ minimum $ map receivedOffset l
         else Wait

-- | Setup a NtpClient and run a application that uses that client.
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

firstAddr :: String -> [AddrInfo] -> IO (Maybe AddrInfo, Maybe AddrInfo)
firstAddr name l = case (find isV4Addr l, find isV6Addr l) of
    (Nothing, Nothing) -> ioError $ userError $ "lookup host failed :" ++ name
    p -> return p
    where
        isV4Addr :: AddrInfo -> Bool
        isV4Addr addr = addrFamily addr == AF_INET

        isV6Addr :: AddrInfo -> Bool
        isV6Addr addr = addrFamily addr == AF_INET6


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
    -> AddrInfo
    -> IO (Either IOError Socket)
createAndBindSock tracer addr someDest = do
    (tryIOError $ Socket.socket (addrFamily addr) Datagram Socket.defaultProtocol) >>= \case
        Left err -> return $ Left err
        Right sock -> (tryIOError $ trySock sock) >>= \case
            Right () -> do
                traceWith tracer $ NtpTraceSocketCreated (show $ addrFamily addr) (show $ addrAddress addr)
                return $ Right sock
            Left err -> do
                Socket.close sock
                return $ Left err
    where
        trySock s = do
            Socket.setSocketOption s ReuseAddr 1
            Socket.bind s (addrAddress addr)
            p <- mkNtpPacket
            void $ Socket.ByteString.sendManyTo s (LBS.toChunks $ encode p)
                                                     (setNtpPort $ Socket.addrAddress someDest)
            traceWith tracer NtpTracePacketSent

socketReaderThread
    :: Tracer IO NtpTrace
    -> TVar (Maybe [ReceivedPacket])
    -> Socket
    -> IO (Either IOError ())
socketReaderThread tracer inQueue socket = tryIOError $ forever $ do
    (bs, _) <- Socket.ByteString.recvFrom socket ntpPacketSize
    t <- getCurrentTime
    case decodeOrFail $ LBS.fromStrict bs of
        Left  (_, _, err) -> traceWith tracer $ NtpTraceSocketReaderDecodeError err
        Right (_, _, packet) -> do
-- todo : filter bad packets, i.e. late packets and spoofed packets
            traceWith tracer NtpTraceReceiveLoopPacketReceived
            let received = ReceivedPacket packet t (clockOffsetPure packet t)
            atomically $ modifyTVar' inQueue $ maybeAppend received
    where
        maybeAppend _ Nothing  = Nothing
        maybeAppend p (Just l) = Just $ p:l

startSocketReaders
    :: Tracer IO NtpTrace
    -> TVar (Maybe [ReceivedPacket])
    -> These (Socket, [AddrInfo]) (Socket, [AddrInfo])
    -> IO (Either IOError ())
startSocketReaders tracer inQueue sockets = case sockets of
    This (v4, _) -> do
        socketReaderThread tracer inQueue v4
    That (v6, _) -> do
         socketReaderThread tracer inQueue v6
    These (v4, _) (v6, _) -> do
        err <- race (socketReaderThread tracer inQueue v4)
                    (socketReaderThread tracer inQueue v6)
        case err of
            (Left  r@(Left e)) -> do
                 traceWith tracer $ NtpTraceSocketReaderIOException e
                 return r
            (Right r@(Left e)) -> do
                 traceWith tracer $ NtpTraceSocketReaderIOException e
                 return r
            _ -> error "unreachable"

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
    -> TVar (Maybe [ReceivedPacket])
    -> These (Socket, [AddrInfo]) (Socket, [AddrInfo])
    -> IO (Either IOError ())
runQueryLoop tracer ntpSettings ntpStatus inQueue servers = tryIOError $ forever $ do
    traceWith tracer NtpTraceClientStartQuery
    void $ atomically $ writeTVar inQueue $ Just []
    (_id, outcome) <- runThreads
    case outcome of
        Timeout -> do
            traceWith tracer NtpTraceUpdateStatusQueryFailed
            atomically $ writeTVar ntpStatus NtpSyncUnavailable
        Result offset -> do
             traceWith tracer $ NtpTraceUpdateStatusClockOffset $ getNtpOffset offset
             atomically $ writeTVar ntpStatus $ NtpDrift offset

    void $ atomically $ writeTVar inQueue Nothing
    traceWith tracer NtpTraceClientSleeping
    threadDelayInterruptible ntpStatus $ fromIntegral $ ntpPollDelay ntpSettings
    where
        runThreads
           = withAsync (sendBoth servers >> loopForever)      $ \sender ->
             withAsync (timeout      >> return Timeout)   $ \delay ->
             withAsync (checkReplies >>= return . Result) $ \revc ->
                    waitAnyCancel [sender, delay, revc]

        timeout = do
            threadDelay $ fromIntegral $ ntpResponseTimeout ntpSettings
            traceWith tracer NtpTraceClientWaitingForRepliesTimeout

        checkReplies = atomically $ do
            q <- readTVar inQueue
            case q of
               Nothing -> error "Queue of incomming packets not available"
               Just l -> case (ntpReportPolicy ntpSettings) l of
                   Wait -> retry
                   Report r -> return r

        sendBoth s = case s of
            This v4 -> send v4
            That v6 -> send v6
            These v4 v6 -> send v6 >> send v4

        send (sock, addrs) = forM_ addrs $ \addr -> do
            p <- mkNtpPacket
            void $ Socket.ByteString.sendManyTo sock (LBS.toChunks $ encode p) (setNtpPort $ Socket.addrAddress addr)
            traceWith tracer NtpTracePacketSent
            
        loopForever = forever $ threadDelay 500_000_000

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

-- | Setup and run the NTP client.
-- In case of an IOError (for example when network interface goes down) cleanup and return.

oneshotClient ::
       Tracer IO NtpTrace
    -> (NtpClientSettings, TVar NtpStatus)
    -> IO ()
oneshotClient tracer (ntpSettings, ntpStatus)
  = (tryIOError $ bracket acquire release action) >>= \case
      Right () -> return ()
      Left err -> traceWith tracer $ NtpTraceOneshotClientIOError err
  where
    action
        :: ( These (Socket, [AddrInfo]) (Socket, [AddrInfo])
           , TVar (Maybe [ReceivedPacket]) )
        -> IO ()
    action (sockets, inQueue) = do -- Todo
        err <- race (startSocketReaders tracer inQueue sockets) --  fst $ undistrPairThese sockets
                    (runQueryLoop tracer ntpSettings ntpStatus inQueue sockets )
        case err of
            (Left  (Left e)) -> traceWith tracer $ NtpTraceSocketReaderIOException e
            (Right (Left e)) -> traceWith tracer $ NtpTraceQueryLoopIOException e
            _ -> error "unreachable"

    acquire :: IO (These (Socket, [AddrInfo]) (Socket, [AddrInfo]), TVar (Maybe [ReceivedPacket]))
    acquire = do
        inQueue <- atomically $ newTVar Nothing
        (v4Servers,   v6Servers)   <- lookupServers $ ntpServers ntpSettings
        (v4LocalAddr, v6LocalAddr) <- udpLocalAddresses >>= firstAddr "localhost"
        v4Resources <- case v4Servers of
            [] -> return Nothing
            l@(remote:_)  -> case v4LocalAddr of
                Nothing -> return Nothing -- ? error or unreachable ?
                Just localAddr -> createAndBindSock tracer localAddr remote >>= \case
                    Right sock -> return $ Just (sock ,l)
                    Left _err -> return Nothing -- ??

        v6Resources <- case v6Servers of
            [] -> return Nothing
            l@(remote:_)  -> case v6LocalAddr of
                Nothing -> return Nothing -- ? error or unreachable ?
                Just localAddr -> createAndBindSock tracer localAddr remote >>= \case
                    Right sock -> return $ Just (sock, l)
                    Left _err -> return Nothing -- ??

        case (v4Resources, v6Resources) of
            (Just v4, Nothing ) -> return (This v4, inQueue)
            (Nothing, Just v6 ) -> return (That v6, inQueue)
            (Just v4, Just v6 ) -> return (These v4 v6, inQueue)
            (Nothing, Nothing ) -> ioError $ userError "Neither IPv4 nor IPv6 available"

    release
        :: (These (Socket, [AddrInfo]) (Socket, [AddrInfo]), TVar (Maybe [ReceivedPacket]))
        -> IO ()
    release (s , _) = case s of
        This (v4, _) -> do
            Socket.close v4
            traceWith tracer NtpTraceSocketClosed
        That (v6, _) -> do
            Socket.close v6
            traceWith tracer NtpTraceSocketClosed
        These (v4, _) (v6, _) -> do
            Socket.close v6
            Socket.close v4
            traceWith tracer NtpTraceSocketClosed

lookupServers :: [String] -> IO ([AddrInfo], [AddrInfo])
lookupServers names = do
   dests <- forM names $ \server -> resolveHost server >>= firstAddr server
   return (mapMaybe fst dests, mapMaybe snd dests)
          
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
        { ntpServers = ["0.de.pool.ntp.org", "0.europe.pool.ntp.org", "0.pool.ntp.org"
                       , "1.pool.ntp.org", "2.pool.ntp.org", "3.pool.ntp.org"]
        , ntpResponseTimeout = fromInteger 5_000_000
        , ntpPollDelay       = fromInteger 300_000_000
        , ntpReportPolicy    = minimumOfThree
        }
