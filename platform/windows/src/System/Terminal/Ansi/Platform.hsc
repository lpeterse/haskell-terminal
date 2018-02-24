{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module System.Terminal.Ansi.Platform
  ( withStandardTerminal
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
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Foreign.C.Types
import           Foreign.Marshal.Alloc         (alloca)
import           Foreign.Ptr                   (Ptr, plusPtr, castPtr)
import           Foreign.Storable
import qualified System.IO                     as IO
import qualified System.IO.Error               as IO

import qualified Control.Monad.Terminal.Events as T
import qualified System.Terminal.Ansi.Internal as T

#include "hs_terminal.h"

withStandardTerminal :: (MonadIO m, MonadMask m) => (T.TerminalEnv -> m a) -> m a
withStandardTerminal action = withTerminalInput $ withTerminalOutput $ do
  mainThread     <- liftIO myThreadId
  interrupt      <- liftIO (newTVarIO False)
  chars          <- liftIO newTChanIO
  events         <- liftIO newTChanIO
  output         <- liftIO newEmptyTMVarIO
  outputFlush    <- liftIO newEmptyTMVarIO
  withOutputProcessing mainThread output outputFlush $
    withInputProcessing mainThread interrupt chars events $ action $
      T.TerminalEnv {
        T.envTermType       = "xterm"
      , T.envInputChars     = readTChan  chars
      , T.envInputEvents    = readTChan  events
      , T.envInterrupt      = swapTVar   interrupt False
      , T.envOutput         = putTMVar   output
      , T.envOutputFlush    = putTMVar   outputFlush ()
      , T.envSpecialChars   = specialChars
      }
  where
    withTerminalInput = bracket
      ( liftIO getConsoleInputModeDesired >>= liftIO . setConsoleInputMode )
      ( liftIO . setConsoleInputMode ) . const

    withTerminalOutput = bracket
      ( liftIO getConsoleOutputModeDesired >>= liftIO . setConsoleOutputMode )
      ( liftIO . setConsoleOutputMode ) . const

    withInputProcessing mainThreadId interrupt chars events ma = do
      terminate  <- liftIO (newTVarIO False)
      terminated <- liftIO (newTVarIO False)
      bracket_
        (liftIO $ forkIO $ processInput mainThreadId terminate terminated interrupt chars events)
        (liftIO (atomically (writeTVar terminate True) >> atomically (readTVar terminated >>= check))) ma

    withOutputProcessing mainThreadId output outputFlush ma = do
      terminate  <- liftIO (newTVarIO False)
      terminated <- liftIO (newTVarIO False)
      bracket_
        (liftIO $ forkIO $ processOutput mainThreadId terminate terminated output outputFlush)
        (liftIO (atomically (writeTVar terminate True) >> atomically (readTVar terminated >>= check))) ma

processOutput :: ThreadId -> TVar Bool -> TVar Bool -> TMVar Text.Text -> TMVar () -> IO ()
processOutput mainThread terminate terminated output outputFlush =
  loop `E.catch` (\e-> E.throwTo mainThread (e:: E.SomeException)) `E.finally` atomically (writeTVar terminated True)
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

processInput :: ThreadId -> TVar Bool -> TVar Bool ->  TVar Bool -> TChan Char -> TChan T.Event -> IO ()
processInput mainThread terminate terminated interrupt chars events =
  -- Synchronous exception will be rethrown to the main thread.
  -- Asynchronous exceptions (apart from `E.AsyncException` thrown by the RTS) won't occur.
  -- In all cases the thread finally writes `True` into the `terminated` variable.
  loop `E.catch` (\e-> E.throwTo mainThread (e:: E.SomeException)) `E.finally` atomically (writeTVar terminated True)
  where
    timeoutMillis :: CULong
    timeoutMillis = 100
    loop :: IO ()
    loop = tryGetConsoleInputEvent >>= \case
      -- `tryGetConsoleInputEvent` is a blocking system call. It cannot be interrupted, but
      -- is guaranteed to return after at most 100ms. In this case it is checked whether
      -- this thread shall either terminate or is allowed to continue.
      -- This is cooperative multitasking to circumvent the limitations of IO on Windows.
      Nothing -> atomically (readTVar terminate) >>= \t-> unless t loop
      Just ev -> (flip (>>)) loop $ case ev of
        KeyEvent { ceKeyChar = c, ceKeyDown = d }
          | c == '\NUL'          -> pure ()
          | c == '\ESC' && not d -> atomically (writeTChan chars '\NUL')
          | c == '\ETX' &&     d -> do 
                                    unhandledInterrupt <- atomically $ do
                                      writeTChan chars '\ETX'
                                      swapTVar interrupt True
                                    when unhandledInterrupt (E.throwTo mainThread E.UserInterrupt)
                                    -- In virtual terminal mode, Windows actually sends Ctrl+C and there is no
                                    -- way a non-responsive application can be killed from keyboard.
                                    -- The solution is to catch this specific event and swap an STM interrupt flag.
                                    -- If the old value is found to be True then it must at least be the second
                                    -- time the user has pressed Ctrl+C _and_ the application was to busy to
                                    -- to reset the interrupt flag in the meantime. In this specific case
                                    -- an asynchronous `E.UserInterrupt` exception is thrown to the main thread
                                    -- and either terminates the application or at least the current computation.
          |                    d -> atomically (writeTChan chars c)
          | otherwise            -> pure () -- Other key release events get swallowed.
        MouseEvent mev           -> atomically $ writeTChan events $ T.MouseEvent mev
        WindowEvent wev          -> atomically $ writeTChan events $ T.WindowEvent wev
        UnknownEvent x           -> atomically $ writeTChan events (T.EvUnknownSequence $ "unknown console input event" ++ show x)
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

specialChars :: Char -> Maybe T.Event
specialChars = \case
  '\r'    -> Just $ T.EvKey T.KEnter []
  '\DEL'  -> Just $ T.EvKey T.KErase []
  '\ESC'  -> Just $ T.EvKey T.KEscape []
  _       -> Nothing

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
    }
  | MouseEvent  T.MouseEvent
  | WindowEvent T.WindowEvent
  | UnknownEvent CUShort
  deriving (Eq, Ord, Show)

-- See https://msdn.microsoft.com/en-us/library/windows/desktop/aa383751(v=vs.85).aspx
instance Storable ConsoleInputEvent where
  sizeOf    _ = (#size struct _INPUT_RECORD)
  alignment _ = (#alignment struct _INPUT_RECORD)
  peek ptr    = peekEventType >>= \case
    (#const KEY_EVENT) -> KeyEvent
      <$> (peek ptrKeyDown >>= \case { 0-> pure False; _-> pure True; })
      <*> (toEnum . fromIntegral <$> peek ptrKeyUnicodeChar)
    (#const MOUSE_EVENT) -> MouseEvent <$> do
      pos <- peek ptrMousePositionX >>= \x-> peek ptrMousePositionY >>= \y-> pure (fromIntegral x, fromIntegral y)
      btn <- peek ptrMouseButtonState
      peek ptrMouseEventFlags >>= \case
        (#const MOUSE_MOVED)    -> pure $ T.MouseMoved pos
        (#const MOUSE_WHEELED)  -> pure $ T.MouseWheeled pos $ if btn > 0 then T.Up    else T.Down
        (#const MOUSE_HWHEELED) -> pure $ T.MouseWheeled pos $ if btn > 0 then T.Right else T.Left 
        _ -> case btn of
          (#const FROM_LEFT_1ST_BUTTON_PRESSED) -> pure $ T.MouseButtonPressed  pos T.LeftButton
          (#const FROM_LEFT_2ND_BUTTON_PRESSED) -> pure $ T.MouseButtonPressed  pos T.OtherButton
          (#const FROM_LEFT_3RD_BUTTON_PRESSED) -> pure $ T.MouseButtonPressed  pos T.OtherButton
          (#const FROM_LEFT_4TH_BUTTON_PRESSED) -> pure $ T.MouseButtonPressed  pos T.OtherButton
          (#const RIGHTMOST_BUTTON_PRESSED)     -> pure $ T.MouseButtonPressed  pos T.RightButton
          _                                     -> pure $ T.MouseButtonReleased pos
    (#const FOCUS_EVENT) -> peek ptrFocus >>= \case
      0 -> pure $ WindowEvent T.WindowLostFocus
      _ -> pure $ WindowEvent T.WindowGainedFocus
    (#const WINDOW_BUFFER_SIZE_EVENT) -> do
      row <- peek ptrWindowSizeX
      col <- peek ptrWindowSizeY
      pure $ WindowEvent $ T.WindowSizeChanged (fromIntegral row, fromIntegral col)
    evt -> pure (UnknownEvent evt)
    where
      peekEventType         = (#peek struct _INPUT_RECORD, EventType) ptr :: IO CUShort
      ptrEvent              = castPtr $ (#ptr struct _INPUT_RECORD, Event) ptr :: Ptr a
      ptrKeyDown            = (#ptr struct _KEY_EVENT_RECORD, bKeyDown) ptrEvent :: Ptr CInt
      ptrKeyUnicodeChar     = (#ptr struct _KEY_EVENT_RECORD, uChar) ptrEvent :: Ptr CWchar
      ptrMousePosition      = (#ptr struct _MOUSE_EVENT_RECORD, dwMousePosition) ptrEvent :: Ptr a
      ptrMousePositionX     = (#ptr struct _COORD, X) ptrMousePosition :: Ptr CShort
      ptrMousePositionY     = (#ptr struct _COORD, Y) ptrMousePosition :: Ptr CShort
      ptrMouseEventFlags    = (#ptr struct _MOUSE_EVENT_RECORD, dwEventFlags) ptrEvent :: Ptr CULong
      ptrMouseButtonState   = (#ptr struct _MOUSE_EVENT_RECORD, dwButtonState) ptrEvent :: Ptr CULong
      ptrWindowSize         = (#ptr struct _WINDOW_BUFFER_SIZE_RECORD, dwSize) ptrEvent :: Ptr a
      ptrWindowSizeX        = (#ptr struct _COORD, X) ptrWindowSize :: Ptr CShort
      ptrWindowSizeY        = (#ptr struct _COORD, Y) ptrWindowSize :: Ptr CShort
      ptrFocus              = (#ptr struct _FOCUS_EVENT_RECORD, bSetFocus) ptrEvent :: Ptr CInt
  poke = undefined

foreign import ccall "hs_wait_console_input"
  unsafeWaitConsoleInput :: CULong -> IO CULong

foreign import ccall "hs_read_console_input"
  unsafeReadConsoleInput :: Ptr ConsoleInputEvent -> IO CInt

foreign import ccall unsafe "hs_get_console_input_mode_desired"
  getConsoleInputModeDesired :: IO Int

foreign import ccall unsafe "hs_set_console_input_mode"
  setConsoleInputMode :: Int -> IO Int

foreign import ccall unsafe "hs_get_console_output_mode_desired"
  getConsoleOutputModeDesired :: IO Int

foreign import ccall unsafe "hs_set_console_output_mode"
  setConsoleOutputMode :: Int -> IO Int

foreign import ccall unsafe "hs_get_console_winsize"
  unsafeGetConsoleWindowSize :: Ptr CInt -> Ptr CInt -> IO CInt
