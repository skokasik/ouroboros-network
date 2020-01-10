module Network.NTP.Trace
where
import           Control.Exception (IOException)
import           Network.NTP.Packet (Microsecond)

data NtpTrace
    = NtpTraceStartNtpClient
    | NtpTraceClientActNow
    | NtpTraceReceiveLoopPacketReceived
    | NtpTraceClientStartQuery
    | NtpTraceUpdateStatusQueryFailed
    | NtpTraceClientSleeping
    | NtpTraceClientWaitingForRepliesTimeout
    | NtpTracePacketSent
    | NtpTraceRestartingClient
    | NtpTraceSocketClosed
    | NtpTraceSocketReaderDecodeError String
    | NtpTraceSocketReaderIOException IOException
    | NtpTraceQueryLoopIOException IOException
    | NtpTraceOneshotClientIOError IOException
    | NtpTraceUpdateStatusClockOffset Microsecond
    | NtpTraceSocketCreated String String
    | NtpTraceRestartDelay Int
    deriving (Show)
