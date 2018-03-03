{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module System.Terminal.Ansi.Platform
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
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Foreign.C.Types
import           Foreign.Marshal.Alloc         (alloca)
import           Foreign.Ptr                   (Ptr, plusPtr, castPtr)
import           Foreign.Storable
import qualified System.IO                     as IO
import qualified System.IO.Error               as IO

import qualified Control.Monad.Terminal.Input as T
import qualified Control.Monad.Terminal.Ansi.AnsiTerminal as T

#include "hs_terminal.h"

withTerminal :: (MonadIO m, MonadMask m) => (T.AnsiTerminal -> m a) -> m a
withTerminal action = do
  mainThread     <- liftIO myThreadId
  interrupt      <- liftIO (newTVarIO False)
  events         <- liftIO newTChanIO
  output         <- liftIO newEmptyTMVarIO
  outputFlush    <- liftIO newEmptyTMVarIO
  screenSize     <- liftIO (newTVarIO =<< getScreenSize)
  withConsoleModes $
    withOutputProcessing mainThread output outputFlush $
      withInputProcessing mainThread interrupt events $ action $
        T.AnsiTerminal {
          T.ansiTermType       = "xterm"
        , T.ansiInputEvents    = readTChan  events
        , T.ansiInterrupt      = swapTVar   interrupt False >>= check
        , T.ansiOutput         = putTMVar   output
        , T.ansiOutputFlush    = putTMVar   outputFlush ()
        , T.ansiScreenSize     = readTVar   screenSize
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
            Just (Just t) -> Text.putStr t       >> loop

withInputProcessing :: (MonadIO m, MonadMask m) => ThreadId -> TVar Bool -> TChan T.Event -> m a -> m a
withInputProcessing mainThread interrupt events ma = do
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
      latestCharacter   <- newTVarIO '\NUL'
      latestMouseButton <- newTVarIO T.LeftButton
      fix $ \continue-> tryGetConsoleInputEvent >>= \case
        -- `tryGetConsoleInputEvent` is a blocking system call. It cannot be interrupted, but
        -- is guaranteed to return after at most 100ms. In this case it is checked whether
        -- this thread shall either terminate or is allowed to continue.
        -- This is cooperative multitasking to circumvent the limitations of IO on Windows.
        Nothing -> do
          shallTerminate <- atomically (readTVar terminate)
          unless shallTerminate continue
        Just ev -> (>> continue) $ case ev of
          KeyEvent { ceKeyChar = c, ceKeyDown = d, ceKeyModifiers = mods }
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
            -- When the escape key is release, a NUL character is written to the
            -- char stream. The NUL character is a replacement for timing based
            -- escape sequence recognition and enables the escape sequence decoder
            -- to reliably distinguish real escape key presses and escape sequences
            -- from another.
            | c == '\ESC' && not d -> atomically $
                writeTChan events (T.KeyEvent (T.KeyChar '\NUL') mempty)
            -- When the character is ESC and the key is pressed down it might be
            -- that the key is hold pressed. In this case a NUL has to be emitted
            -- before emitting the ESC in order to signal that the previous ESC does
            -- not introduce a sequence.
            | c == '\ESC' &&     d -> atomically $ do
                readTVar latestCharacter >>= \case
                  '\ESC' -> writeTChan events (T.KeyEvent (T.KeyChar '\NUL') mempty)
                  _      -> writeTVar  latestCharacter '\ESC'
                writeTChan events (T.KeyEvent (T.KeyChar '\ESC') mempty)
            | d -> atomically $ writeTVar latestCharacter c >> case c of
                '\NUL'  -> pure ()
                '\t'    -> writeTChan events (T.KeyEvent T.KeyTab      mods)
                '\r'    -> writeTChan events (T.KeyEvent T.KeyEnter    mods)
                '\n'    -> writeTChan events (T.KeyEvent T.KeyEnter    mods)
                '\SP'   -> writeTChan events (T.KeyEvent T.SpaceKey    mods)
                '\BS'   -> writeTChan events (T.KeyEvent T.KeyDelete   mods)
                '\DEL'  -> writeTChan events (T.KeyEvent T.KeyErase    mods)
                _       -> writeTChan events (T.KeyEvent (T.KeyChar c) mods)
            | otherwise -> pure () -- All other key events shall be ignored.
          MouseEvent mouseEvent -> case mouseEvent of
            T.MouseButtonPressed _  btn -> atomically $ do
              writeTChan events $ T.MouseEvent mouseEvent
              writeTVar latestMouseButton btn
            T.MouseButtonReleased pos _ -> atomically $ do
              btn <- readTVar latestMouseButton
              writeTChan events $ T.MouseEvent $ T.MouseButtonReleased pos btn
              writeTChan events $ T.MouseEvent $ T.MouseButtonClicked  pos btn
            _ -> atomically $ 
              writeTChan events $ T.MouseEvent mouseEvent
          WindowEvent wev -> atomically $ writeTChan events $ T.WindowEvent wev
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

getScreenSize :: IO (Int,Int)
getScreenSize =
  alloca $ \rowsPtr-> alloca $ \colsPtr->
    unsafeGetConsoleWindowSize rowsPtr colsPtr >>= \case
      0 -> do
        rows <- fromIntegral <$> peek rowsPtr
        cols <- fromIntegral <$> peek colsPtr
        pure (rows, cols)
      i -> pure (0,0)

data ConsoleInputEvent
  = KeyEvent
    { ceKeyDown            :: Bool
    , ceKeyChar            :: Char
    , ceKeyModifiers       :: T.Modifiers
    }
  | MouseEvent  T.MouseEvent
  | WindowEvent T.WindowEvent
  | UnknownEvent WORD
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
      pos <- peek ptrMousePositionX >>= \x-> peek ptrMousePositionY >>= \y-> pure (fromIntegral x, fromIntegral y)
      btn <- peek ptrMouseButtonState
      peek ptrMouseEventFlags >>= \case
        (#const MOUSE_MOVED)    -> pure (T.MouseMoved   pos)
        (#const MOUSE_WHEELED)  -> pure (T.MouseWheeled pos $ if btn .&. 0xff000000 > 0 then T.Down  else T.Up)
        (#const MOUSE_HWHEELED) -> pure (T.MouseWheeled pos $ if btn .&. 0xff000000 > 0 then T.Right else T.Left)
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
    (#const WINDOW_BUFFER_SIZE_EVENT) -> do
      row <- peek ptrWindowSizeX
      col <- peek ptrWindowSizeY
      pure $ WindowEvent $ T.WindowSizeChanged (fromIntegral row, fromIntegral col)
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
      ptrWindowSize         = (#ptr struct _WINDOW_BUFFER_SIZE_RECORD, dwSize) ptrEvent   :: Ptr a
      ptrWindowSizeX        = (#ptr struct _COORD, X) ptrWindowSize                       :: Ptr SHORT
      ptrWindowSizeY        = (#ptr struct _COORD, Y) ptrWindowSize                       :: Ptr SHORT
      ptrFocus              = (#ptr struct _FOCUS_EVENT_RECORD, bSetFocus) ptrEvent       :: Ptr BOOL
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

foreign import ccall unsafe "hs_get_console_winsize"
  unsafeGetConsoleWindowSize :: Ptr SHORT -> Ptr SHORT -> IO BOOL

-- See https://msdn.microsoft.com/en-us/library/windows/desktop/aa383751(v=vs.85).aspx
-- for how Windows data types translate to stdint types.

type BOOL  = CInt
type SHORT = CShort
type WORD  = CUShort
type DWORD = CULong
