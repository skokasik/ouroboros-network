{-# LANGUAGE CApiFFI             #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InterruptibleFFI    #-}

module System.Win32.CompletionPort
  ( createIoCompletionPort
  , getQueuedCompletionStatus
  , postQueuedCompletionStatus
  ) where

import Data.Word (Word64)
import Foreign.Marshal (maybeWith, with)
import Foreign.C.Types (CUIntPtr (..))

import System.Win32.NamedPipes
import System.Win32.Types

#include <fcntl.h>
#include <windows.h>

-- | Windows documentation:
-- https://docs.microsoft.com/en-us/windows/win32/fileio/createiocompletionport
--
createIoCompletionPort
    :: HANDLE -- ^ file handle to associate with the completion port, can be
              -- 'iNVALID_HANDLE_VALUE'
    -> HANDLE -- ^ existing completion port, can be 'nullPtr'
    -> Word64 -- ^ completion key
    -> DWORD  -- ^ number of concurrent threads
    -> IO HANDLE
createIoCompletionPort fileHandle completionPort completionKey concurrentThreads
    = c_CreateIoCompletionPort fileHandle completionPort (CUIntPtr completionKey) concurrentThreads

foreign import ccall unsafe "windows.h CreateIoCompletionPort"
    c_CreateIoCompletionPort :: HANDLE -> HANDLE -> ULONG_PTR -> DWORD -> IO HANDLE

-- | Windows documentation:
-- https://docs.microsoft.com/en-us/windows/win32/api/ioapiset/nf-ioapiset-getqueuedcompletionstatus
--
-- Also this part of IO Completion Ports documentation
-- https://docs.microsoft.com/en-us/windows/win32/fileio/i-o-completion-ports
-- is relevant:
--
-- Although any number of threads can call GetQueuedCompletionStatus for a
-- specified I/O completion port, when a specified thread calls
-- GetQueuedCompletionStatus the first time, it becomes associated with the
-- specified I/O completion port until one of three things occurs: The thread
-- exits, specifies a different I/O completion port, or closes the I/O
-- completion port. In other words, a single thread can be associated with, at
-- most, one I/O completion port.
--
getQueuedCompletionStatus
    :: HANDLE
    -> DWORD
    -> Word64
    -> OVERLAPPED
    -> DWORD
    -> IO ()
getQueuedCompletionStatus completionPort lpNumberOfBytesTransferred
                          lpCompletionKey lpOverlapped dwMilliseconds =
    with lpNumberOfBytesTransferred $ \numPtr ->
      with lpOverlapped $ \ovlPtr ->
        failIfFalse_ "GetQueuedCompletionStatus"
          $ c_GetQueuedCompletionStatus
              completionPort numPtr (CUIntPtr lpCompletionKey)
              ovlPtr dwMilliseconds

foreign import ccall safe "windows.h GetQueuedCompletionStatus"
    c_GetQueuedCompletionStatus :: HANDLE -> LPDWORD -> ULONG_PTR
                                -> LPOVERLAPPED -> DWORD -> IO BOOL

postQueuedCompletionStatus
    :: HANDLE
    -> DWORD
    -> Word64
    -> Maybe OVERLAPPED
    -> IO ()
postQueuedCompletionStatus completionPort dwNumberOfBytesTransferred
                           dwCompletionKey lpOverlapped =
    maybeWith with lpOverlapped $ \lpOverlappedPtr ->
      failIfFalse_ "PostQueuedCompletionStatus"
        $ c_PostQueuedCompletionStatus completionPort dwNumberOfBytesTransferred
                                       (CUIntPtr dwCompletionKey) lpOverlappedPtr


foreign import ccall safe "windows.h PostQueuedCompletionStatus"
    c_PostQueuedCompletionStatus :: HANDLE -> DWORD -> ULONG_PTR -> LPOVERLAPPED -> IO Bool
