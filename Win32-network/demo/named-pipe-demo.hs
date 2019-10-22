{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import System.IO
import System.Exit
import           System.Win32 (HANDLE)
import qualified System.Win32 as Win32
import qualified System.Win32.NamedPipes as Win32
import qualified System.Win32.File.Interruptible as Win32
import System.Environment

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ["server"] -> server
    ["client"] -> client
    _          -> usage

usage :: IO ()
usage = do
  hPutStr stderr "usage: named-pipe-demo [server|client]"
  exitFailure

pipeName :: String
pipeName = "\\\\.\\pipe\\named-pipe-demo"

putStrLn_ :: String -> IO ()
putStrLn_ = BSC.putStrLn . BSC.pack

server :: IO ()
server = do

  putStrLn_ "creating pipe..."
  hpipe <- Win32.createNamedPipe pipeName
                                 Win32.pIPE_ACCESS_DUPLEX
                                 (Win32.pIPE_TYPE_BYTE .|. Win32.pIPE_READMODE_BYTE)
                                 Win32.pIPE_UNLIMITED_INSTANCES
                                 512
                                 512
                                 0
                                 Nothing
  putStrLn_ "created pipe, waiting for client"
  Win32.connectNamedPipe hpipe Nothing
  putStrLn_ "client connected"
  _ <- forkIO $ do
         putStrLn_ "starting client conversation"
         serverLoop hpipe
           `finally` (do putStrLn "client disconnected"
                         Win32.closeHandle hpipe)
  threadDelay 1
  server


encodeMsg :: String -> ByteString
encodeMsg msg = BSC.pack (msg ++ "\n")

serverLoop :: HANDLE -> IO ()
serverLoop hpipe = do
  _ <- Win32.writeHandle hpipe (encodeMsg "Hi! >") Nothing
  putStrLn "Sent prompt, awaiting reply"
  resp <- Win32.hGetLine hpipe
  putStrLn $ "received: " ++ show resp
  let reply = "reversed: " ++ show (reverse resp)
  putStrLn $ "replying: " ++ show reply
  _ <- Win32.writeHandle hpipe (encodeMsg reply) Nothing
  serverLoop hpipe

client :: IO ()
client = do
  hpipe <- Win32.createFile pipeName
                            (Win32.gENERIC_READ .|. Win32.gENERIC_WRITE)
                            Win32.fILE_SHARE_NONE
                            Nothing
                            Win32.oPEN_EXISTING
                            Win32.fILE_ATTRIBUTE_NORMAL
                            Nothing
  putStrLn "opened pipe"
  clientLoop hpipe
    `finally` Win32.closeHandle hpipe

clientLoop :: HANDLE -> IO ()
clientLoop hpipe = do
  putStrLn "awaiting prompt..."
  prompt <- Win32.hGetLine hpipe
  putStrLn prompt
  reply <- getLine
  case reply of
    "quit" -> return ()
    _      -> do putStrLn $ "sending reply: " ++ show reply
                 _ <- Win32.writeHandle hpipe (encodeMsg reply) Nothing
                 putStrLn "reply sent"
                 putStrLn "reply flushed"
                 resp <- Win32.hGetLine hpipe
                 putStrLn $ "response: " ++ resp
                 clientLoop hpipe

