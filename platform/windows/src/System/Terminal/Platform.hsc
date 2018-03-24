{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module System.Terminal.Platform
  ( withTerminal
  ) where

import           Control.Concurrent            (ThreadId, myThreadId, forkIO)
import           Control.Concurrent.STM.TChan  (TChan, newTChanIO, readTChan, writeTChan)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar   (TVar, newTVarIO, readTVar, swapTVar, writeTVar)
import qualified Control.Exception             as E
import           Control.Monad                 (forever, void, when, unless)
import           Control.Monad.Catch           (MonadMask, bracket, bracket_)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.STM             (STM, atomically, check, orElse)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bits
import           Data.Function                 (fix)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Text.Encoding            as Text
import           Foreign.C.Types
import           Foreign.Marshal.Alloc         (alloca)
import           Foreign.Ptr                   (Ptr, plusPtr, castPtr)
import           Foreign.Storable
import qualified System.IO                     as IO
import qualified System.IO.Error               as IO

import qualified Control.Monad.Terminal.Input as T
import qualified Control.Monad.Terminal.Terminal as T

#include "hs_terminal.h"

withTerminal :: (MonadIO m, MonadMask m) => (T.Terminal -> m a) -> m a
withTerminal action = do
  mainThread     <- liftIO myThreadId
  interrupt      <- liftIO (newTVarIO False)
  events         <- liftIO newTChanIO
  output         <- liftIO newEmptyTMVarIO
  outputFlush    <- liftIO newEmptyTMVarIO
  screenSize     <- liftIO (newTVarIO =<< getConsoleScreenSize)
  withConsoleModes $
    withOutputProcessing mainThread output outputFlush $
      withInputProcessing mainThread interrupt events screenSize $ action $
        T.Terminal {
          T.termType           = "xterm" -- They claim it behaves like xterm although this is certainly a bit ambituous.
        , T.termInput          = readTChan  events
        , T.termOutput         = putTMVar   output
        , T.termInterrupt      = swapTVar   interrupt False >>= check
        , T.termFlush          = putTMVar   outputFlush ()
        , T.termScreenSize     = readTVar   screenSize
        , T.termSpecialChars   = \case
            '\r'   -> Just $ T.KeyEvent T.EnterKey mempty
            '\t'   -> Just $ T.KeyEvent T.TabKey mempty
            '\SP'  -> Just $ T.KeyEvent T.SpaceKey mempty
            '\b'   -> Just $ T.KeyEvent T.BackspaceKey mempty
            '\DEL' -> Just $ T.KeyEvent T.BackspaceKey mempty
            _      -> Nothing
        }

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

withOutputProcessing :: (MonadIO m, MonadMask m) => ThreadId -> TMVar Text.Text -> TMVar () -> m a -> m a
withOutputProcessing mainThread output outputFlush ma = do
  terminate  <- liftIO (newTVarIO False)
  terminated <- liftIO (newTVarIO False)
  bracket_
    (liftIO $ forkIO $ run terminate terminated)
    (liftIO (atomically (writeTVar terminate True) >> atomically (readTVar terminated >>= check))) ma
  where
    run :: TVar Bool -> TVar Bool -> IO ()
    run terminate terminated =
      -- Synchronous exception will be rethrown to the main thread.
      -- Asynchronous exceptions (apart from `E.AsyncException` thrown by the RTS) won't occur.
      -- In all cases the thread finally writes `True` into the `terminated` variable.
      (loop `E.catch` (\e-> E.throwTo mainThread (e:: E.SomeException))) `E.finally` atomically (writeTVar terminated True)
      where
        loop :: IO ()
        loop = do
          x <- atomically $ (readTVar terminate >>= check >> pure Nothing)
                  `orElse` (Just . Just <$> takeTMVar output)
                  `orElse` (takeTMVar outputFlush >> pure (Just Nothing))
          case x of
            Nothing       -> pure ()
            Just Nothing  -> IO.hFlush IO.stdout >> loop
            Just (Just t) -> putText t >> loop

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

withInputProcessing :: (MonadIO m, MonadMask m) => ThreadId -> TVar Bool -> TChan T.Event -> TVar (Int,Int) -> m a -> m a
withInputProcessing mainThread interrupt events screenSize ma = do
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
      latestMouseButton      <- newTVarIO T.LeftButton
      fix $ \continue-> tryGetConsoleInputEvent >>= \case
        -- `tryGetConsoleInputEvent` is a blocking system call. It cannot be interrupted, but
        -- is guaranteed to return after at most 100ms. In this case it is checked whether
        -- this thread shall either terminate or is allowed to continue.
        -- This is cooperative multitasking to circumvent the limitations of IO on Windows.
        Nothing -> do
          shallTerminate <- atomically (readTVar terminate)
          unless shallTerminate $ do
            -- The NUL character is a replacement for timing based
            -- escape sequence recognition and enables the escape sequence decoder
            -- to reliably distinguish real escape key presses and escape sequences
            -- from another. A NUL is added after each timeout potentially
            -- terminating any ambiguous (escape) sequences.
            atomically $ do
              latest <- readTVar latestCharacter
              when (latest /= '\NUL') $ do
                writeTVar latestCharacter '\NUL'
                writeTChan events (T.KeyEvent (T.CharKey '\NUL') mempty)
            continue
        Just ev -> (>> continue) $ case ev of
          KeyEvent { ceCharKey = c, ceKeyDown = d, ceKeyModifiers = mods }
            -- In virtual terminal mode, Windows actually sends Ctrl+C and there is no
            -- way a non-responsive application can be killed from keyboard.
            -- The solution is to catch this specific event and swap an STM interrupt flag.
            -- If the old value is found to be True then it must at least be the second
            -- time the user has pressed Ctrl+C _and_ the application was to busy to
            -- to reset the interrupt flag in the meantime. In this specific case
            -- an asynchronous `E.UserInterrupt` exception is thrown to the main thread
            -- and either terminates the application or at least the current computation.
            | c == '\ETX' &&     d -> do 
                unhandledInterrupt <- atomically $ do
                  writeTVar latestCharacter '\ETX'
                  writeTChan events T.InterruptEvent
                  swapTVar interrupt True
                when unhandledInterrupt (E.throwTo mainThread E.UserInterrupt)
            -- When the character is ESC and the key is pressed down it might be
            -- that the key is hold pressed. In this case a NUL has to be emitted
            -- before emitting the ESC in order to signal that the previous ESC does
            -- not introduce a sequence.
            | c == '\ESC' &&     d -> atomically $ do
                readTVar latestCharacter >>= \case
                  '\ESC' -> writeTChan events (T.KeyEvent (T.CharKey '\NUL') mempty)
                  _      -> writeTVar  latestCharacter '\ESC'
                writeTChan events (T.KeyEvent (T.CharKey '\ESC') mempty)
            | d -> atomically $ do
                writeTVar latestCharacter c
                writeTChan events (T.KeyEvent (T.CharKey c) mods)
            | otherwise -> pure () -- All other key events shall be ignored.
          MouseEvent mouseEvent -> case mouseEvent of
            T.MouseButtonPressed (r,c) btn -> atomically $ do
              csbi <- readTVar latestScreenBufferInfo
              writeTChan events $ T.MouseEvent $ T.MouseButtonPressed (r - srWindowTop csbi, c - srWindowLeft csbi) btn
              writeTVar latestMouseButton btn
            T.MouseButtonReleased (r,c) _ -> atomically $ do
              csbi <- readTVar latestScreenBufferInfo
              btn <- readTVar latestMouseButton
              writeTChan events $ T.MouseEvent $ T.MouseButtonReleased (r - srWindowTop csbi, c - srWindowLeft csbi) btn
              writeTChan events $ T.MouseEvent $ T.MouseButtonClicked  (r - srWindowTop csbi, c - srWindowLeft csbi) btn
            T.MouseButtonClicked (r,c) btn -> atomically $ do
              csbi <- readTVar latestScreenBufferInfo
              writeTChan events $ T.MouseEvent $ T.MouseButtonClicked (r - srWindowTop csbi, c - srWindowLeft csbi) btn
            T.MouseWheeled (r,c) dir -> atomically $ do
              csbi <- readTVar latestScreenBufferInfo
              writeTChan events $ T.MouseEvent $ T.MouseWheeled (r - srWindowTop csbi, c - srWindowLeft csbi) dir
            T.MouseMoved (r,c) -> atomically $ do
              csbi <- readTVar latestScreenBufferInfo
              writeTChan events $ T.MouseEvent $ T.MouseMoved (r - srWindowTop csbi, c - srWindowLeft csbi)

          WindowEvent wev -> case wev of
            T.WindowSizeChanged _ -> do
              csbi <- getConsoleScreenBufferInfo
              atomically $ do
                writeTVar latestScreenBufferInfo csbi
                let sz = (srWindowBottom csbi - srWindowTop csbi + 1, srWindowRight csbi - srWindowLeft csbi + 1)
                sz' <- swapTVar screenSize sz
                -- Observation: Not every event is an actual change to the screen size.
                -- Only real changes shall be passed.
                when (sz /= sz') (writeTChan events $ T.WindowEvent $ T.WindowSizeChanged sz)
            _ -> atomically $ writeTChan events $ T.WindowEvent wev
          UnknownEvent x  -> atomically $ writeTChan events (T.OtherEvent $ "Unknown console input event " ++ show x ++ ".")

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

getConsoleScreenSize :: IO (Int, Int)
getConsoleScreenSize = do
  csbi <- getConsoleScreenBufferInfo
  pure (srWindowBottom csbi - srWindowTop csbi + 1, srWindowRight csbi - srWindowLeft csbi + 1)

data ConsoleInputEvent
  = KeyEvent
    { ceKeyDown            :: Bool
    , ceCharKey            :: Char
    , ceKeyModifiers       :: T.Modifiers
    }
  | MouseEvent  T.MouseEvent
  | WindowEvent T.WindowEvent
  | UnknownEvent WORD
  deriving (Eq, Ord, Show)

data ConsoleScreenBufferInfo
  = ConsoleScreenBufferInfo
  { srWindowLeft   :: Int
  , srWindowTop    :: Int
  , srWindowRight  :: Int
  , srWindowBottom :: Int
  }
  deriving (Eq, Ord, Show)

modifiersFromControlKeyState :: DWORD -> T.Modifiers
modifiersFromControlKeyState dw = a $ b $ c $ d $ e mempty
  where
    a = if (#const LEFT_ALT_PRESSED)   .&. dw == 0 then id else mappend T.altKey
    b = if (#const LEFT_CTRL_PRESSED)  .&. dw == 0 then id else mappend T.ctrlKey
    c = if (#const RIGHT_ALT_PRESSED)  .&. dw == 0 then id else mappend T.altKey
    d = if (#const RIGHT_CTRL_PRESSED) .&. dw == 0 then id else mappend T.ctrlKey
    e = if (#const SHIFT_PRESSED)      .&. dw == 0 then id else mappend T.shiftKey

instance Storable ConsoleInputEvent where
  sizeOf    _ = (#size struct _INPUT_RECORD)
  alignment _ = (#alignment struct _INPUT_RECORD)
  peek ptr    = peekEventType >>= \case
    (#const KEY_EVENT) -> KeyEvent
      <$> (peek ptrKeyDown >>= \case { 0-> pure False; _-> pure True; })
      <*> (toEnum . fromIntegral <$> peek ptrKeyUnicodeChar)
      <*> (modifiersFromControlKeyState <$> peek ptrKeyControlKeyState)
    (#const MOUSE_EVENT) -> MouseEvent <$> do
      pos <- peek ptrMousePositionX >>= \x-> peek ptrMousePositionY >>= \y-> pure (fromIntegral y, fromIntegral x)
      btn <- peek ptrMouseButtonState
      peek ptrMouseEventFlags >>= \case
        (#const MOUSE_MOVED)    -> pure (T.MouseMoved   pos)
        (#const MOUSE_WHEELED)  -> pure (T.MouseWheeled pos $ if btn .&. 0xff000000 > 0 then T.Downwards  else T.Upwards)
        (#const MOUSE_HWHEELED) -> pure (T.MouseWheeled pos $ if btn .&. 0xff000000 > 0 then T.Rightwards else T.Leftwards)
        _ -> case btn of
          (#const FROM_LEFT_1ST_BUTTON_PRESSED) -> pure $ T.MouseButtonPressed  pos T.LeftButton
          (#const FROM_LEFT_2ND_BUTTON_PRESSED) -> pure $ T.MouseButtonPressed  pos T.OtherButton
          (#const FROM_LEFT_3RD_BUTTON_PRESSED) -> pure $ T.MouseButtonPressed  pos T.OtherButton
          (#const FROM_LEFT_4TH_BUTTON_PRESSED) -> pure $ T.MouseButtonPressed  pos T.OtherButton
          (#const RIGHTMOST_BUTTON_PRESSED)     -> pure $ T.MouseButtonPressed  pos T.RightButton
          _                                     -> pure $ T.MouseButtonReleased pos T.OtherButton
    (#const FOCUS_EVENT) -> peek ptrFocus >>= \case
      0 -> pure $ WindowEvent T.WindowLostFocus
      _ -> pure $ WindowEvent T.WindowGainedFocus
    (#const WINDOW_BUFFER_SIZE_EVENT) ->
      pure $ WindowEvent $ T.WindowSizeChanged (0,0)
    evt -> pure (UnknownEvent evt)
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
    where
      peek' x           = fromIntegral <$> peek x
      ptrSrWindow       = (#ptr struct _CONSOLE_SCREEN_BUFFER_INFO, srWindow) ptr :: Ptr a
      ptrSrWindowLeft   = (#ptr struct _SMALL_RECT, Left)   ptrSrWindow           :: Ptr SHORT
      ptrSrWindowTop    = (#ptr struct _SMALL_RECT, Top)    ptrSrWindow           :: Ptr SHORT
      ptrSrWindowRight  = (#ptr struct _SMALL_RECT, Right)  ptrSrWindow           :: Ptr SHORT
      ptrSrWindowBottom = (#ptr struct _SMALL_RECT, Bottom) ptrSrWindow           :: Ptr SHORT
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
