module Network.NTP.Trace
where
import           Control.Exception (IOException)
import           Network.NTP.Packet (Microsecond)

data NtpTrace
    = NtpTraceStartNtpClient
    | NtpTraceClientActNow
    | NtpTraceClientForceCheck
    | NtpTraceClientAbort
    | NtpTraceUpdateStatusNoResponses
    | NtpTraceUpdateStatusClockOffset Microsecond
    | NtpTraceSendLoopCollectedAllResponses
    | NtpTraceSpawnNtpClientStarting
    | NtpTraceSpawnNtpClientStarted
    | NtpTraceSpawnNtpClientSocketsClosed
    | NtpTraceSpawnNtpClientResolveDNS
    | NtpTraceSpawnNtpClientResolvePending
    | NtpTraceReceiveLoopDecodeError String
    | NtpTraceReceiveLoopHandleIOException IOException
    | NtpTraceReceiveLoopException
    | NtpTraceReceiveLoopLatePacket
    | NtpTraceReceiveLoopPacketReceived
    | NtpTraceReceiveLoopPacketDeltaTime Microsecond
    | NtpTraceMkSocketsNoSockets
    | NtpTraceMkSocketsIOExecption IOException
    | NtpTraceResolvHostIOException IOException
    | NtpTraceResolveHostNotResolved String
    | NtpTraceResolveHostResolved String -- todo also log addr
    | NtpTraceSocketCreated String String
    | NtpTraceSendPacketNoMatchingSocket String String
    | NtpTraceSentToIOException String IOException
    | NtpTraceSentTryResend String
    | NtpTraceSentNotRetrying
-- new code
    | NtpTraceSocketReaderDecodeError String
    | NtpTraceClientStartQuery
    | NtpTraceClientSleeping
    | NtpTraceClientWaitingForRepliesTimeout
    | NtpTraceUpdateStatusQueryFailed

    deriving (Show)
