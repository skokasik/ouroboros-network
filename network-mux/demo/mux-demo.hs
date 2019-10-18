{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Demo application which for now is only using mux over named pipes on
-- Windows.
--
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import Control.Tracer (Tracer (..), showTracing)
import qualified Data.ByteString.Char8 as BSC
import Data.Bits
import Data.ByteString (ByteString)
import Data.Void

import qualified Network.Mux.Interface as Mx
import qualified Network.Mux.Bearer.Pipe as Mx

import Test.Mux.ReqResp

import System.Win32
import System.Win32.NamedPipes
import System.Win32.CompletionPort

import System.IO
import System.Exit
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["server"]       -> echoServer
      ["client", n, msgSize] -> client (read n) (read msgSize)
      _                -> usage

usage :: IO ()
usage = do
  hPutStr stderr $ "usage: mux-demo server\n"
                 ++"       mux-demo client (n :: Int) (msgSize :: Int)"
  exitFailure

pipeName :: String
pipeName = "\\\\.\\pipe\\mux-demo"

putStrLn_ :: String -> IO ()
putStrLn_ = BSC.putStrLn . BSC.pack

debugTracer :: Show a => Tracer IO a
debugTracer = showTracing (Tracer putStrLn_)

--
-- Protocols
--

data ReqRespProtocol = ReqRespPr
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum ReqRespProtocol where
    fromProtocolEnum ReqRespPr = 2
    toProtocolEnum 2 = Just ReqRespPr
    toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits ReqRespProtocol where
    maximumMessageSize  ReqRespPr = maxBound
    maximumIngressQueue ReqRespPr = maxBound

data PeerId = Client | Server
  deriving Show

--
-- server: accept loop, server loop
--


-- | Server accept loop.
--
echoServer :: IO ()
echoServer = do
    putStrLn_ "creating pipe..."
    hpipe <- createNamedPipe pipeName
                             (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             1024
                             1024
                             0
                             Nothing
    putStrLn_ $ "created pipe " ++ show hpipe ++ ", waiting for client"
    connectNamedPipe hpipe Nothing
    port <- createIoCompletionPort iNVALID_HANDLE_VALUE nullPtr 0 maxBound
    putStrLn_ "client connected"
    _ <- forkIO $ do
           putStrLn_ "starting client conversation"
           serverLoop hpipe port
             `finally` (do putStrLn "client disconnected"
                           closePipe hpipe)
    threadDelay 1
    echoServer


serverLoop :: HANDLE
           -> HANDLE
           -> IO ()
serverLoop h {- port -} = do
    let tracer = debugTracer
    -- pipeChannel <- Mx.pipeChannelFromHANDLE h port
    pipeChannel <- Mx.pipeChannelFromHANDLE_Ev h
    Mx.runMuxWithPipes
        tracer
        Server
        app
        pipeChannel
  where
    app :: Mx.MuxApplication 'Mx.ResponderApp PeerId ReqRespProtocol IO Void ()
    app = Mx.MuxResponderApplication $ \_ ReqRespPr channel -> do
      runServer debugTracer channel serverApp

    serverApp :: ReqRespServer ByteString ByteString IO ()
    serverApp = ReqRespServer {
        recvMsgReq  = \req -> pure (req, serverApp),
        recvMsgDone = pure ()
      }


--
-- client
--
    

client :: Int -> Int -> IO ()
client n msgSize = do
    let tracer = debugTracer
    h <- createFile pipeName
                    (gENERIC_READ .|. gENERIC_WRITE)
                    fILE_SHARE_NONE
                    Nothing
                    oPEN_EXISTING
                    fILE_FLAG_OVERLAPPED
                    Nothing
    putStrLn_ $ "createFile: " ++ show h
    port <- createIoCompletionPort iNVALID_HANDLE_VALUE nullPtr 0 maxBound
    pipeChannel <- Mx.pipeChannelFromHANDLE h port
    -- pipeChannel <- Mx.pipeChannelFromHANDLE_Ev h
    Mx.runMuxWithPipes
        tracer
        Server
        app
        pipeChannel
  where
    app :: Mx.MuxApplication 'Mx.InitiatorApp PeerId ReqRespProtocol IO () Void
    app = Mx.MuxInitiatorApplication $ \_ ReqRespPr channel -> do
      runClient debugTracer channel (clientApp n (BSC.pack (replicate msgSize 'a')))

    clientApp :: Int -> ByteString -> ReqRespClient ByteString ByteString IO ()
    clientApp 0 _ = SendMsgDone (pure ())
    clientApp m msg = SendMsgReq msg
                                 (pure . clientApp (pred m)) -- send back request
                             
