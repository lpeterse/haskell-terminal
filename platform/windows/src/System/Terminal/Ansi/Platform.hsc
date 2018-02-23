{-# LANGUAGE LambdaCase, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module System.Terminal.Ansi.Platform
  ( withStandardTerminal
  ) where

import           Control.Concurrent            (ThreadId, myThreadId, threadDelay)
import           Control.Concurrent.Async      (async, cancel, withAsync)
import           Control.Concurrent.STM.TChan  (TChan, newTChanIO, readTChan, writeTChan)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar   (TVar, newTVarIO, readTVar, swapTVar, writeTVar)
import           Control.Monad                 (forever, void, when)
import           Control.Monad.Catch           (MonadMask, bracket)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.STM             (STM, atomically, check, orElse)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bits
import           Foreign.C.Types
import           Foreign.Marshal.Alloc         (alloca)
import           Foreign.Ptr                   (Ptr, plusPtr, castPtr)
import           Foreign.Storable
import qualified Control.Exception             as E
import qualified Data.ByteString               as BS
import qualified Data.Text.IO                  as Text
import qualified System.IO                     as IO
import qualified System.IO.Error               as IO

import qualified Control.Monad.Terminal.Events as T
import qualified System.Terminal.Ansi.Internal as T

#include "hs_terminal.h"

withStandardTerminal :: (MonadIO m, MonadMask m) => (T.TerminalEnv -> m a) -> m a
withStandardTerminal action = withTerminalInput $ withTerminalOutput $ do
  mainThreadId   <- liftIO myThreadId
  interruptFlag  <- liftIO (newTVarIO False)
  chars          <- liftIO newTChanIO
  events         <- liftIO newTChanIO
  output         <- liftIO newEmptyTMVarIO
  outputFlush    <- liftIO newEmptyTMVarIO
  screenSize     <- liftIO (newTVarIO =<< getScreenSize)
  cursorPosition <- liftIO newEmptyTMVarIO
  withResizeMonitoring screenSize events $
    withOutputProcessing mainThreadId output outputFlush $ 
      withInputProcessing mainThreadId interruptFlag cursorPosition chars events $ action $
        T.TerminalEnv {
          T.envTermType       = "vt101"
        , T.envInputChars     = readTChan  chars
        , T.envInputEvents    = readTChan  events
        , T.envOutput         = putTMVar   output
        , T.envOutputFlush    = putTMVar   outputFlush ()
        , T.envSpecialChars   = specialChars
        --, T.envInterrupt      = swapTVar interruptFlag False >>= check
        --, T.envScreenSize     = readTVar screenSize
        --, T.envCursorPosition = takeTMVar cursorPosition
        }
  where
    withTerminalInput = bracket
      ( liftIO getConsoleInputModeDesired >>= liftIO . setConsoleInputMode )
      ( liftIO . setConsoleInputMode ) . const
    withTerminalOutput = bracket
      ( liftIO getConsoleOutputModeDesired >>= liftIO . setConsoleOutputMode )
      ( liftIO . setConsoleOutputMode ) . const
    withOutputProcessing mainThreadId output outputFlush = bracket
      (liftIO $ async run)
      (liftIO . cancel) . const
      where
        run = forever $ atomically ((Left <$> takeTMVar output) `orElse` (Right <$> takeTMVar outputFlush)) >>= \case
            Left  t -> Text.putStr t
            Right _ -> IO.hFlush IO.stdout

    -- The "\ESC[0" trick certainly requires some explanation:
    -- IO on Windows is blocking which means that a thread blocked
    -- on reading from a file descriptor cannot be killed/canceled
    -- and any other thread trying to cancel it will be blocked, too.
    -- The observed behavior here was that the cleanup procedure waited
    -- indefinitely for the input processing thread to die after having
    -- send it an exception.
    -- The solution is to send this specific escape sequence which asks
    -- the terminal to report its device attributes. This incoming event
    -- unblocks the input processing thread which then dies immediately.
    withInputProcessing mainThreadId interruptFlag cursorPosition chars events = bracket
      (liftIO $ async $ processInput mainThreadId interruptFlag cursorPosition chars events)
      (\a-> liftIO $ withAsync trigger $ const $ cancel a) . const
      where
        trigger = do
          threadDelay 100000 -- Yes, it's a race..
          IO.hPutStr IO.stdout "\ESC[0c"
          IO.hFlush  IO.stdout
    withResizeMonitoring screenSize events = bracket
      (liftIO $ async monitor)
      (liftIO . cancel) . const
      where
        monitor = forever $ do
          threadDelay 100000
          new <- getScreenSize
          old <- atomically $ readTVar screenSize
          when (new /= old) $ atomically $ do
            writeTVar screenSize new
            writeTChan events $ T.EvResize new

processInput :: ThreadId -> TVar Bool -> TMVar (Int, Int) -> TChan Char -> TChan T.Event -> IO ()
processInput mainThreadId interruptFlag cursorPosition chars events =
  forever $ getConsoleInputEvent >>= \case
    KeyEvent { ceKeyChar = c, ceKeyDown = d }
      | c == '\NUL'          -> pure ()
      | c == '\ESC' && not d -> atomically (writeTChan chars '\NUL')
      |                    d -> atomically (writeTChan chars c)
      | otherwise            -> pure ()
    MouseEvent mev           -> atomically $ writeTChan events $ T.MouseEvent mev
    FocusEvent {}            -> atomically $ writeTChan events T.FocusEvent
    WindowBufferSizeEvent {} -> atomically $ writeTChan events $ T.EvResize (0,0)
    UnknownEvent x           -> atomically $ writeTChan events (T.EvUnknownSequence $ "unknown console input event" ++ show x)

{-
    runAnsiDecoder :: TMVar Char -> IO ()
    runAnsiDecoder = runReaderT ma 
      where
        CharStreamM ma = forever $ do
          event <- mapEvent <$> T.decodeAnsi
          CharStreamM $ liftIO $ atomically $ writeTChan chan event
          -- In virtual terminal mode, Windows actually sends Ctrl+C and there is no
          -- way a non-responsive application can be killed from keyboard.
          -- The solution is to catch this specific event and swap an STM interrupt flag.
          -- If the old value is found to be True then it must at least be the second
          -- time the user has pressed Ctrl+C _and_ the application was to busy to
          -- to reset the interrupt flag in the meantime. In this specific case
          -- an asynchronous `E.UserInterrupt` exception is thrown to the main thread
          -- and either terminates the application or at least the current computation.
          when (event == T.EvKey (T.KChar 'C') [T.MCtrl]) $ CharStreamM $ liftIO $ do
            unhandledInterrupt <- atomically (swapTVar interruptFlag True)
            when unhandledInterrupt (E.throwTo mainThreadId E.UserInterrupt)
-}

specialChars :: Char -> Maybe T.Event
specialChars = \case
  '\r'    -> Just $ T.EvKey T.KEnter []
  '\DEL'  -> Just $ T.EvKey T.KErase []
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

newtype ControlKeyState = ControlKeyState CULong deriving (Eq,Ord,Show)

data ConsoleInputEvent
  = KeyEvent
    { ceKeyDown            :: Bool
    , ceKeyRepeatCount     :: Int
    , ceKeyChar            :: Char
    , ceKeyControlKeyState :: ControlKeyState
    }
  | MouseEvent T.MouseEvent
  | FocusEvent
  | WindowBufferSizeEvent
  | UnknownEvent CUShort
  deriving (Eq, Ord, Show)

-- See https://msdn.microsoft.com/en-us/library/windows/desktop/aa383751(v=vs.85).aspx
instance Storable ConsoleInputEvent where
  sizeOf    _ = (#size struct _INPUT_RECORD)
  alignment _ = (#alignment struct _INPUT_RECORD)
  peek ptr    = peekEventType >>= \case
    (#const KEY_EVENT) -> KeyEvent
      <$> (peek ptrKeyDown >>= \case { 0-> pure False; _-> pure True; })
      <*> (fromIntegral <$> peek ptrKeyRepeatCount)
      <*> (toEnum . fromIntegral <$> peek ptrKeyUnicodeChar)
      <*> (ControlKeyState <$> peek ptrKeyControlKeyState)
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
    (#const FOCUS_EVENT) -> pure FocusEvent
    (#const WINDOW_BUFFER_SIZE_EVENT) -> pure WindowBufferSizeEvent
    evt -> pure (UnknownEvent evt)
    where
      peekEventType         = (#peek struct _INPUT_RECORD, EventType) ptr :: IO CUShort
      ptrEvent              = castPtr $ (#ptr struct _INPUT_RECORD, Event) ptr :: Ptr a
      ptrKeyDown            = (#ptr struct _KEY_EVENT_RECORD, bKeyDown) ptrEvent :: Ptr CInt
      ptrKeyRepeatCount     = (#ptr struct _KEY_EVENT_RECORD, wRepeatCount) ptrEvent :: Ptr CUShort
      ptrKeyUnicodeChar     = (#ptr struct _KEY_EVENT_RECORD, uChar) ptrEvent :: Ptr CWchar
      ptrKeyControlKeyState = (#ptr struct _KEY_EVENT_RECORD, dwControlKeyState) ptrEvent :: Ptr CULong
      ptrMousePosition      = (#ptr struct _MOUSE_EVENT_RECORD, dwMousePosition) ptrEvent :: Ptr a
      ptrMousePositionX     = (#ptr struct _COORD, X) ptrMousePosition :: Ptr CShort
      ptrMousePositionY     = (#ptr struct _COORD, Y) ptrMousePosition :: Ptr CShort
      ptrMouseEventFlags    = (#ptr struct _MOUSE_EVENT_RECORD, dwEventFlags) ptrEvent :: Ptr CULong
      ptrMouseButtonState   = (#ptr struct _MOUSE_EVENT_RECORD, dwButtonState) ptrEvent :: Ptr CULong
  poke = undefined

foreign import ccall unsafe "hs_read_console_input"
  unsafeReadConsoleInput :: Ptr ConsoleInputEvent -> IO CInt

getConsoleInputEvent :: IO ConsoleInputEvent
getConsoleInputEvent =
  alloca $ \ptr->
    unsafeReadConsoleInput ptr >>= \case
      0 -> peek ptr
      _ -> E.throwIO (IO.userError "getConsoleInputEvent: error reading console events")

isCapsLockOn :: ControlKeyState -> Bool
isCapsLockOn (ControlKeyState x) = x .&. (#const CAPSLOCK_ON) /= 0

isEnhancedKey :: ControlKeyState -> Bool
isEnhancedKey (ControlKeyState x) = x .&. (#const ENHANCED_KEY) /= 0

isLeftAltPressed :: ControlKeyState -> Bool
isLeftAltPressed (ControlKeyState x) = x .&. (#const LEFT_ALT_PRESSED) /= 0

isLeftCtrlPressed :: ControlKeyState -> Bool
isLeftCtrlPressed (ControlKeyState x) = x .&. (#const LEFT_CTRL_PRESSED) /= 0

isNumLockOn :: ControlKeyState -> Bool
isNumLockOn (ControlKeyState x) = x .&. (#const NUMLOCK_ON) /= 0

isRightAltPressed :: ControlKeyState -> Bool
isRightAltPressed (ControlKeyState x) = x .&. (#const RIGHT_ALT_PRESSED) /= 0

isRightCtrlPressed :: ControlKeyState -> Bool
isRightCtrlPressed (ControlKeyState x) = x .&. (#const RIGHT_CTRL_PRESSED) /= 0

isScrollLockOn :: ControlKeyState -> Bool
isScrollLockOn (ControlKeyState x) = x .&. (#const SCROLLLOCK_ON) /= 0

isShiftPressed :: ControlKeyState -> Bool
isShiftPressed (ControlKeyState x) = x .&. (#const SHIFT_PRESSED) /= 0
