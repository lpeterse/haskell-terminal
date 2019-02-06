module System.Terminal.Platform
  ( LocalTerminal ()
  , withTerminal
  ) where

import           Control.Applicative           ((<|>))
import           Control.Concurrent            (ThreadId, myThreadId, forkIO)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar   (TVar, newTVarIO, readTVar, readTVarIO, swapTVar, writeTVar)
import qualified Control.Exception             as E
import           Control.Monad                 (forM_, when, unless)
import           Control.Monad.Catch           (MonadMask, bracket, bracket_)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.STM             (STM, atomically, check)
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Foreign.C.Types
import           Foreign.Marshal.Alloc         (alloca)
import           Foreign.Ptr                   (Ptr, plusPtr, castPtr)
import           Foreign.Storable
import qualified System.IO                     as IO
import qualified System.IO.Error               as IO

import           System.Terminal.MonadInput
import           System.Terminal.Terminal
import           System.Terminal.Decoder
import           System.Terminal.Encoder

#include "hs_terminal.h"

data LocalTerminal
    = LocalTerminal
    { localType              :: BS.ByteString
    , localEvent             :: STM Event
    , localInterrupt         :: STM Interrupt
    }

instance Terminal LocalTerminal where
    termType                = localType
    termEvent               = localEvent
    termInterrupt           = localInterrupt
    termCommand _ c         = putText (defaultEncode c)
    termFlush _             = pure ()
    termGetWindowSize _     = getConsoleWindowSize
    termGetCursorPosition _ = getConsoleCursorPosition

withTerminal :: (MonadIO m, MonadMask m) => (LocalTerminal -> m a) -> m a
withTerminal action = do
    mainThread     <- liftIO myThreadId
    interrupt      <- liftIO (newTVarIO False)
    windowChanged  <- liftIO (newTVarIO False)
    events         <- liftIO newEmptyTMVarIO
    withConsoleModes $
        withInputProcessing mainThread interrupt windowChanged events $ action $ LocalTerminal
            { localType = "xterm" -- They claim it behaves like xterm although this is certainly a bit ambituous.
            , localInterrupt =
                swapTVar interrupt False >>= check >> pure Interrupt
            , localEvent = do
                changed <- swapTVar windowChanged False
                if changed
                    then pure (WindowEvent WindowSizeChanged)
                    else takeTMVar events
            }

decoder0 :: Decoder
decoder0 = defaultDecoder f
    where
        f mods = \case
            '\r'   -> Just $ KeyEvent EnterKey     mods
            '\t'   -> Just $ KeyEvent TabKey       mods
            '\SP'  -> Just $ KeyEvent SpaceKey     mods
            '\b'   -> Just $ KeyEvent BackspaceKey mods
            '\DEL' -> Just $ KeyEvent BackspaceKey mods
            _      -> Nothing

withConsoleModes :: (MonadIO m, MonadMask m) => m a -> m a
withConsoleModes = bracket before after . const
  where
    modeInput m0 = m7
      where
        m1 = m0 .|. (#const ENABLE_VIRTUAL_TERMINAL_INPUT)
        m2 = m1 .|. (#const ENABLE_MOUSE_INPUT)
        m3 = m2 .|. (#const ENABLE_WINDOW_INPUT)
        m4 = m3 .|. (#const ENABLE_EXTENDED_FLAGS)
        m5 = m4 .&. complement (#const ENABLE_LINE_INPUT)
        m6 = m5 .&. complement (#const ENABLE_PROCESSED_INPUT)
        m7 = m6 .&. complement (#const ENABLE_QUICK_EDIT_MODE)
    modeOutput m0 = m1
      where
        m1 = m0 .|. (#const ENABLE_VIRTUAL_TERMINAL_PROCESSING)
    before = liftIO $ do
      i <- getConsoleInputMode
      o <- getConsoleOutputMode
      setConsoleInputMode  (modeInput  i)
      setConsoleOutputMode (modeOutput o)
      pure (i, o)
    after (i, o) = liftIO $ do
      setConsoleInputMode i
      setConsoleOutputMode o
    setConsoleInputMode mode = do
      r <- unsafeSetConsoleInputMode mode
      -- TODO: Function reports error, but nonetheless has the correct effect. Windows bug?
      when (r == 0) $ pure () -- E.throwIO (IO.userError "setConsoleInputMode: not a tty?")
    setConsoleOutputMode mode = do
      r <- unsafeSetConsoleOutputMode mode
      when (r == 0) $ E.throwIO (IO.userError "setConsoleOutputMode: not a tty?")
    getConsoleInputMode = alloca $ \ptr-> do
      r <- unsafeGetConsoleInputMode ptr
      when (r == 0) $ E.throwIO (IO.userError "getConsoleInputMode: not a tty?")
      peek ptr
    getConsoleOutputMode = alloca $ \ptr-> do
      r <- unsafeGetConsoleOutputMode ptr
      when (r == 0) $ E.throwIO (IO.userError "getConsoleOutputMode: not a tty?")
      peek ptr

putText :: Text.Text -> IO ()
putText text = do
  -- First, flush everything that is in the regular output buffer.
  -- Just in case the user uses the regular output opertions
  -- it is desirable to interleave with it as little as possible.
  IO.hFlush IO.stdout
  alloca $ put (Text.encodeUtf16LE text)
  where
    -- Windows expects Unicode encoded as UTF16LE.
    -- This _does not_ mean that every character is just 2 bytes long.
    -- Consider the character '\\x1d11e': Its encoded form is
    -- 11011000 00110100 11011101 00011110 (4 bytes).
    -- The underlying `writeConsoleW` function reports the UTF-16 encoding
    -- units (2 bytes) written and not the bytes written.
    put bs ptrWritten
      | BS.null bs = pure ()
      | otherwise  = do
          (r,len) <- BS.unsafeUseAsCStringLen bs $ \(ptr,len2)-> do
            let len = fromIntegral (len2 `div` 2)
            r <- unsafeWriteConsole ptr len ptrWritten
            pure (r,len)
          when (r == 0) $ E.throwIO (IO.userError "putText: not a tty?")
          written <- peek ptrWritten
          when (written < len) (put (BS.drop (fromIntegral len * 2) bs) ptrWritten)

withInputProcessing :: (MonadIO m, MonadMask m) => ThreadId -> TVar Bool -> TVar Bool -> TMVar Event -> m a -> m a
withInputProcessing mainThread interrupt windowChanged events ma = do
  terminate  <- liftIO (newTVarIO False)
  terminated <- liftIO (newTVarIO False)
  bracket_
    (liftIO $ forkIO $ runUntilTermination terminate terminated)
    (liftIO (atomically (writeTVar terminate True) >> atomically (readTVar terminated >>= check))) ma
  where
    runUntilTermination :: TVar Bool -> TVar Bool -> IO ()
    runUntilTermination terminate terminated =
      (run terminate `E.catch` (\e-> E.throwTo mainThread (e:: E.SomeException))) `E.finally` atomically (writeTVar terminated True)

    run :: TVar Bool -> IO ()
    run terminate = do
        latestScreenBufferInfo <- newTVarIO =<< getConsoleScreenBufferInfo
        latestCharacter        <- newTVarIO '\NUL'
        latestMouseButton      <- newTVarIO LeftMouseButton
        latestWindowSize       <- newTVarIO =<< getConsoleWindowSize
        let continue :: Decoder -> IO ()
            continue decoder = do
                shallTerminate <- readTVarIO terminate
                unless shallTerminate (waitForEvents decoder)
            pushEvent :: Event -> IO ()
            pushEvent ev = atomically do -- unblock when thread shall terminate
                putTMVar events ev <|> (readTVar terminate >>= check)
            waitForEvents :: Decoder -> IO ()
            waitForEvents decoder = tryGetConsoleInputEvent >>= \case
                -- `tryGetConsoleInputEvent` is a blocking system call. It cannot be interrupted, but
                -- is guaranteed to return after at most 100ms. In this case it is checked whether
                -- this thread shall either terminate or is allowed to continue.
                -- This is cooperative multitasking to circumvent the limitations of IO on Windows.
                Nothing ->
                    -- The NUL character is a replacement for timing based
                    -- escape sequence recognition and enables the escape sequence decoder
                    -- to reliably distinguish real escape key presses and escape sequences
                    -- from another. A NUL is added after each timeout potentially
                    -- terminating any ambiguous (escape) sequences.
                    case feedDecoder decoder mempty '\NUL' of
                        Left decoder' -> continue decoder'
                        Right evs     -> forM_ evs pushEvent >> continue decoder0
                Just ev -> case ev of
                    ConsoleKeyEvent { ceCharKey = c, ceKeyDown = d, ceKeyModifiers = mods }
                        -- In virtual terminal mode, Windows actually sends Ctrl+C and there is no
                        -- way a non-responsive application can be killed from keyboard.
                        -- The solution is to catch this specific event and swap an STM interrupt flag.
                        -- If the old value is found to be True then it must at least be the second
                        -- time the user has pressed Ctrl+C _and_ the application was to busy to
                        -- to reset the interrupt flag in the meantime. In this specific case
                        -- an asynchronous `E.UserInterrupt` exception is thrown to the main thread
                        -- and either terminates the application or at least the current computation.
                        | c == '\ETX' && d -> do 
                            unhandledInterrupt <- atomically (swapTVar interrupt True)
                            when unhandledInterrupt (E.throwTo mainThread E.UserInterrupt)
                        -- When the character is ESC and the key is pressed down it might be
                        -- that the key is hold pressed. In this case a NUL has to be emitted
                        -- before emitting the ESC in order to signal that the previous ESC does
                        -- not introduce a sequence.
                        | c == '\ESC' && d -> do
                            latest <- readTVarIO latestCharacter
                            case feedDecoder decoder mods (if latest == '\ESC' then '\NUL' else '\ESC') of
                                Left decoder' -> continue decoder'
                                Right evs     -> forM_ evs pushEvent >> continue decoder0
                        | d -> do
                            atomically (writeTVar latestCharacter c)
                            case feedDecoder decoder mods c of
                                Left decoder' -> continue decoder'
                                Right evs     -> forM_ evs pushEvent >> continue decoder0
                        | otherwise -> continue decoder -- All other key events shall be ignored.
                    ConsoleMouseEvent mouseEvent -> case mouseEvent of
                        MouseButtonPressed (r,c) btn -> do
                            csbi <- readTVarIO latestScreenBufferInfo
                            atomically (writeTVar latestMouseButton btn)
                            pushEvent $ MouseEvent $ MouseButtonPressed (r - srWindowTop csbi - 1, c - srWindowLeft csbi - 1) btn
                            continue decoder
                        MouseButtonReleased (r,c) _ -> do
                            csbi <- readTVarIO latestScreenBufferInfo
                            btn <- readTVarIO latestMouseButton
                            pushEvent $ MouseEvent $ MouseButtonReleased (r - srWindowTop csbi - 1, c - srWindowLeft csbi - 1) btn
                            pushEvent $ MouseEvent $ MouseButtonClicked  (r - srWindowTop csbi - 1, c - srWindowLeft csbi - 1) btn
                            continue decoder
                        MouseButtonClicked (r,c) btn -> do
                            csbi <- readTVarIO latestScreenBufferInfo
                            pushEvent $ MouseEvent $ MouseButtonClicked (r - srWindowTop csbi - 1, c - srWindowLeft csbi - 1) btn
                            continue decoder
                        MouseWheeled (r,c) dir -> do
                            csbi <- readTVarIO latestScreenBufferInfo
                            pushEvent $ MouseEvent $ MouseWheeled (r - srWindowTop csbi - 1, c - srWindowLeft csbi - 1) dir
                            continue decoder
                        MouseMoved (r,c) -> do
                            csbi <- readTVarIO latestScreenBufferInfo
                            pushEvent $ MouseEvent $ MouseMoved (r - srWindowTop csbi - 1, c - srWindowLeft csbi - 1)
                            continue decoder
                    ConsoleWindowEvent wev -> case wev of
                        WindowSizeChanged -> do
                            sz <- readTVarIO latestWindowSize
                            sz' <- getConsoleWindowSize
                            -- Observation: Far more events than actual changes to the window size are
                            -- reported when resizing the window. Only pass actual changes.
                            when (sz /= sz') $ atomically do
                                writeTVar latestWindowSize sz'
                                writeTVar windowChanged True
                            continue decoder
                        _ -> do
                            pushEvent (WindowEvent wev)
                            continue decoder
                    ConsoleUnknownEvent x  -> do
                        pushEvent (OtherEvent $ "Unknown console input event " ++ show x ++ ".")
                        continue decoder
        continue decoder0

    timeoutMillis :: CULong
    timeoutMillis = 100

    -- Wait at most `timeoutMillis` for the handle to signal readyness.
    -- Then either read one console event or return `Nothing`.
    tryGetConsoleInputEvent :: IO (Maybe ConsoleInputEvent)
    tryGetConsoleInputEvent =
      unsafeWaitConsoleInput timeoutMillis >>= \case
        (#const WAIT_TIMEOUT)  -> pure Nothing    -- Timeout occured.
        (#const WAIT_OBJECT_0) -> alloca $ \ptr-> -- Handle signaled readyness.
              unsafeReadConsoleInput ptr >>= \case
                0 -> Just <$> peek ptr
                _ -> E.throwIO (IO.userError "getConsoleInputEvent: error reading console events")
        _ -> E.throwIO (IO.userError "getConsoleInputEvent: error waiting for console events")

getConsoleScreenBufferInfo :: IO ConsoleScreenBufferInfo
getConsoleScreenBufferInfo = alloca $ \ptr->
  unsafeGetConsoleScreenBufferInfo ptr >>= \case
    0 -> E.throwIO (IO.userError "getConsoleScreenBufferInfo: not a tty?")
    _ -> peek ptr

getConsoleWindowSize :: IO (Rows, Cols)
getConsoleWindowSize = do
  csbi <- getConsoleScreenBufferInfo
  pure (srWindowBottom csbi - srWindowTop csbi + 1, srWindowRight csbi - srWindowLeft csbi + 1)

getConsoleCursorPosition :: IO (Row, Col)
getConsoleCursorPosition = do
  sbi <- getConsoleScreenBufferInfo
  pure (cpY sbi - srWindowTop sbi, cpX sbi - srWindowLeft sbi)

data ConsoleInputEvent
  = ConsoleKeyEvent
    { ceKeyDown            :: Bool
    , ceCharKey            :: Char
    , ceKeyModifiers       :: Modifiers
    }
  | ConsoleMouseEvent  MouseEvent
  | ConsoleWindowEvent WindowEvent
  | ConsoleUnknownEvent WORD
  deriving (Eq, Ord, Show)

data ConsoleScreenBufferInfo
  = ConsoleScreenBufferInfo
  { srWindowLeft   :: !Int
  , srWindowTop    :: !Int
  , srWindowRight  :: !Int
  , srWindowBottom :: !Int
  , cpX            :: !Int
  , cpY            :: !Int
  }
  deriving (Eq, Ord, Show)

modifiersFromControlKeyState :: DWORD -> Modifiers
modifiersFromControlKeyState dw = a $ b $ c $ d $ e mempty
  where
    a = if (#const LEFT_ALT_PRESSED)   .&. dw == 0 then id else mappend altKey
    b = if (#const LEFT_CTRL_PRESSED)  .&. dw == 0 then id else mappend ctrlKey
    c = if (#const RIGHT_ALT_PRESSED)  .&. dw == 0 then id else mappend altKey
    d = if (#const RIGHT_CTRL_PRESSED) .&. dw == 0 then id else mappend ctrlKey
    e = if (#const SHIFT_PRESSED)      .&. dw == 0 then id else mappend shiftKey

instance Storable ConsoleInputEvent where
  sizeOf    _ = (#size struct _INPUT_RECORD)
  alignment _ = (#alignment struct _INPUT_RECORD)
  peek ptr    = peekEventType >>= \case
    (#const KEY_EVENT) -> ConsoleKeyEvent
      <$> (peek ptrKeyDown >>= \case { 0-> pure False; _-> pure True; })
      <*> (toEnum . fromIntegral <$> peek ptrKeyUnicodeChar)
      <*> (modifiersFromControlKeyState <$> peek ptrKeyControlKeyState)
    (#const MOUSE_EVENT) -> ConsoleMouseEvent <$> do
      pos <- peek ptrMousePositionX >>= \x-> peek ptrMousePositionY >>= \y-> pure (fromIntegral y, fromIntegral x)
      btn <- peek ptrMouseButtonState
      peek ptrMouseEventFlags >>= \case
        (#const MOUSE_MOVED)    -> pure (MouseMoved   pos)
        (#const MOUSE_WHEELED)  -> pure (MouseWheeled pos $ if btn .&. 0xff000000 > 0 then Downwards  else Upwards)
        (#const MOUSE_HWHEELED) -> pure (MouseWheeled pos $ if btn .&. 0xff000000 > 0 then Rightwards else Leftwards)
        _ -> case btn of
          (#const FROM_LEFT_1ST_BUTTON_PRESSED) -> pure $ MouseButtonPressed  pos LeftMouseButton
          (#const FROM_LEFT_2ND_BUTTON_PRESSED) -> pure $ MouseButtonPressed  pos OtherMouseButton
          (#const FROM_LEFT_3RD_BUTTON_PRESSED) -> pure $ MouseButtonPressed  pos OtherMouseButton
          (#const FROM_LEFT_4TH_BUTTON_PRESSED) -> pure $ MouseButtonPressed  pos OtherMouseButton
          (#const RIGHTMOST_BUTTON_PRESSED)     -> pure $ MouseButtonPressed  pos RightMouseButton
          _                                     -> pure $ MouseButtonReleased pos OtherMouseButton
    (#const FOCUS_EVENT) -> peek ptrFocus >>= \case
      0 -> pure $ ConsoleWindowEvent WindowLostFocus
      _ -> pure $ ConsoleWindowEvent WindowGainedFocus
    (#const WINDOW_BUFFER_SIZE_EVENT) ->
      pure $ ConsoleWindowEvent WindowSizeChanged
    evt -> pure (ConsoleUnknownEvent evt)
    where
      peekEventType         = (#peek struct _INPUT_RECORD, EventType) ptr                 :: IO WORD
      ptrEvent              = castPtr $ (#ptr struct _INPUT_RECORD, Event) ptr            :: Ptr a
      ptrKeyDown            = (#ptr struct _KEY_EVENT_RECORD, bKeyDown) ptrEvent          :: Ptr BOOL
      ptrKeyUnicodeChar     = (#ptr struct _KEY_EVENT_RECORD, uChar) ptrEvent             :: Ptr CWchar
      ptrKeyControlKeyState = (#ptr struct _KEY_EVENT_RECORD, dwControlKeyState) ptrEvent :: Ptr DWORD
      ptrMousePosition      = (#ptr struct _MOUSE_EVENT_RECORD, dwMousePosition) ptrEvent :: Ptr a
      ptrMousePositionX     = (#ptr struct _COORD, X) ptrMousePosition                    :: Ptr SHORT
      ptrMousePositionY     = (#ptr struct _COORD, Y) ptrMousePosition                    :: Ptr SHORT
      ptrMouseEventFlags    = (#ptr struct _MOUSE_EVENT_RECORD, dwEventFlags)  ptrEvent   :: Ptr DWORD
      ptrMouseButtonState   = (#ptr struct _MOUSE_EVENT_RECORD, dwButtonState) ptrEvent   :: Ptr DWORD
      ptrFocus              = (#ptr struct _FOCUS_EVENT_RECORD, bSetFocus) ptrEvent       :: Ptr BOOL
  poke = undefined

instance Storable ConsoleScreenBufferInfo where
  sizeOf    _ = (#size struct _CONSOLE_SCREEN_BUFFER_INFO)
  alignment _ = (#alignment struct _CONSOLE_SCREEN_BUFFER_INFO)
  peek ptr    = ConsoleScreenBufferInfo
    <$> peek' ptrSrWindowLeft
    <*> peek' ptrSrWindowTop
    <*> peek' ptrSrWindowRight
    <*> peek' ptrSrWindowBottom
    <*> peek' ptrDwCursorPositionX
    <*> peek' ptrDwCursorPositionY
    where
      peek' x                 = fromIntegral <$> peek x
      ptrSrWindow             = (#ptr struct _CONSOLE_SCREEN_BUFFER_INFO, srWindow) ptr         :: Ptr a
      ptrSrWindowLeft         = (#ptr struct _SMALL_RECT, Left)   ptrSrWindow                   :: Ptr SHORT
      ptrSrWindowTop          = (#ptr struct _SMALL_RECT, Top)    ptrSrWindow                   :: Ptr SHORT
      ptrSrWindowRight        = (#ptr struct _SMALL_RECT, Right)  ptrSrWindow                   :: Ptr SHORT
      ptrSrWindowBottom       = (#ptr struct _SMALL_RECT, Bottom) ptrSrWindow                   :: Ptr SHORT
      ptrDwCursorPosition     = (#ptr struct _CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition) ptr :: Ptr b
      ptrDwCursorPositionX    = (#ptr struct _COORD, X) ptrDwCursorPosition                     :: Ptr SHORT
      ptrDwCursorPositionY    = (#ptr struct _COORD, Y) ptrDwCursorPosition                     :: Ptr SHORT
  poke = undefined

foreign import ccall "hs_wait_console_input"
  unsafeWaitConsoleInput     :: DWORD -> IO DWORD

foreign import ccall "hs_read_console_input"
  unsafeReadConsoleInput     :: Ptr ConsoleInputEvent -> IO BOOL

foreign import ccall unsafe "hs_get_console_input_mode"
  unsafeGetConsoleInputMode  :: Ptr DWORD -> IO BOOL

foreign import ccall unsafe "hs_set_console_input_mode"
  unsafeSetConsoleInputMode  :: DWORD -> IO BOOL

foreign import ccall unsafe "hs_get_console_output_mode"
  unsafeGetConsoleOutputMode :: Ptr DWORD -> IO BOOL

foreign import ccall unsafe "hs_set_console_output_mode"
  unsafeSetConsoleOutputMode :: DWORD -> IO BOOL

foreign import ccall unsafe "hs_get_console_screen_buffer_info"
  unsafeGetConsoleScreenBufferInfo :: Ptr ConsoleScreenBufferInfo -> IO BOOL

foreign import ccall unsafe "hs_write_console"
  unsafeWriteConsole         :: Ptr a -> DWORD -> Ptr DWORD -> IO BOOL

-- See https://msdn.microsoft.com/en-us/library/windows/desktop/aa383751(v=vs.85).aspx
-- for how Windows data types translate to stdint types.

type BOOL  = CInt
type SHORT = CShort
type WORD  = CUShort
type DWORD = CULong
