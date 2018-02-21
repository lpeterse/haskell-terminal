{-# LANGUAGE LambdaCase #-}
module System.Terminal.Ansi.FFI where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc

#include "hs_terminal.h"

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

data ConsoleInputRecord
  = KeyEventRecord
    { --ceKeyDown        :: Bool
    --, ceKeyRepeatCount :: Int
     ceKeyChar        :: Char
    --, ceKeyCt
    }
  | MouseEventRecord
  | FocusEventRecord
  | WindowBufferSizeRecord
  | UnknownEventRecord CUShort
  deriving (Eq, Ord, Show)

-- See https://msdn.microsoft.com/en-us/library/windows/desktop/aa383751(v=vs.85).aspx
instance Storable ConsoleInputRecord where
  sizeOf    _ = (#size struct _INPUT_RECORD)
  alignment _ = (#alignment struct _INPUT_RECORD)
  peek ptr    = peekEventType >>= \case
    (#const KEY_EVENT) -> KeyEventRecord
      <$> (toEnum . fromIntegral <$> peek ptrKeyUnicodeChar)
    (#const MOUSE_EVENT) -> pure MouseEventRecord
    (#const FOCUS_EVENT) -> pure FocusEventRecord
    (#const WINDOW_BUFFER_SIZE_EVENT) -> pure WindowBufferSizeRecord
    evt -> pure (UnknownEventRecord evt)
    where
      peekEventType      = (#peek struct _INPUT_RECORD, EventType) ptr :: IO CUShort
      ptrEvent           = castPtr $ (#ptr struct _INPUT_RECORD, Event) ptr :: Ptr a
      ptrKeyUnicodeChar  = (#ptr struct _KEY_EVENT_RECORD, uChar) ptrEvent :: Ptr CWchar
  poke = undefined

foreign import ccall unsafe "hs_read_console_input"
  unsafeReadConsoleInput :: Ptr ConsoleInputRecord -> IO CInt

getConsoleInputRecord :: IO (Maybe ConsoleInputRecord)
getConsoleInputRecord =
  alloca $ \ptr->
    unsafeReadConsoleInput ptr >>= \case
      0 -> Just <$> peek ptr
      _ -> pure Nothing


