{-# LANGUAGE CApiFFI          #-}

module System.Win32.Event
  ( LPSECURITY_ATTRIBUTES
  , createEvent
  , setEvent
  , resetEvent
  ) where

import Foreign.C.String (withCAString)

import System.Win32.File (LPSECURITY_ATTRIBUTES)
import System.Win32.Types

#include <fcntl.h>
#include <windows.h>


{-
data SECURITY_ATTRIBUTES = SECURITY_ATTRIBUTES
  { saLength             :: DWORD
  , saSecurityDescriptor :: LPVOID
  , saInheritHandle      :: BOOL
  }

instance Storable SECURITY_ATTRIBUTES where
    sizeOf = const (#size SECURITY_ATTRIBUTES)
    alignment _ = #alignment SECURITY_ATTRIBUTES
    poke buf bhi = do
        (#poke SECURITY_ATTRIBUTES, nLength)              buf (saLength bhi)
        (#poke SECURITY_ATTRIBUTES, lpSecurityDescriptor) buf (saSecurityDescriptor bhi)
        (#poke SECURITY_ATTRIBUTES, bInheritHandle)       buf (saInheritHandle bhi)
    peek buf = do
        len <- (#peek SECURITY_ATTRIBUTES, nLength)              buf
        sec <- (#peek SECURITY_ATTRIBUTES, lpSecurityDescriptor) buf
        inh <- (#peek SECURITY_ATTRIBUTES, bInheritHandle)       buf
        return $ SECURITY_ATTRIBUTES len sec inh
-}

createEvent :: Maybe LPSECURITY_ATTRIBUTES
            -> BOOL   -- ^ bManualReset
            -> BOOL   -- ^ bInitialState
            -> String -- ^ lpName
            -> IO HANDLE
createEvent mb_attr bManualReset bInitialState lpName =
  withCAString lpName $ \c_lpName -> 
    c_CreateEvent (maybePtr mb_attr) bManualReset bInitialState c_lpName

foreign import ccall unsafe "windows.h CreateEventA"
  c_CreateEvent :: LPSECURITY_ATTRIBUTES -- ^ lpEventAttributes
                -> BOOL                  -- ^ bManualReset
                -> BOOL                  -- ^ bInitialState
                -> LPCSTR                -- ^ lpName
                -> IO HANDLE

setEvent :: HANDLE -> IO ()
setEvent = failIfFalse_ "SetEvent" . c_SetEvent

foreign import ccall unsafe "windows.h SetEvent"
  c_SetEvent :: HANDLE -> IO BOOL

resetEvent :: HANDLE -> IO ()
resetEvent = failIfFalse_ "ResetEvent" . c_ResetEvent

foreign import ccall unsafe "windows.h ResetEvent"
  c_ResetEvent :: HANDLE -> IO BOOL
