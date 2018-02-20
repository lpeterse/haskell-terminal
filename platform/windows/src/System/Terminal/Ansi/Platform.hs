{-# LANGUAGE LambdaCase #-}
module System.Terminal.Ansi.Platform
  ( withTerminal
  ) where

import           Control.Concurrent            (ThreadId, myThreadId,
                                                threadDelay)
import           Control.Concurrent.Async      (async, cancel, withAsync)
import           Control.Concurrent.STM.TChan  (TChan, newTChanIO, readTChan,
                                                writeTChan)
import           Control.Concurrent.STM.TVar   (TVar, newTVarIO, readTVar,
                                                swapTVar, writeTVar)
import qualified Control.Exception             as E
import           Control.Monad                 (forever, void, when)
import           Control.Monad.Catch           (MonadMask, bracket)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.STM             (STM, atomically, check, orElse)
import           Control.Monad.Trans.State
import qualified Data.ByteString               as BS
import           Foreign.C.Types
import           Foreign.Marshal.Alloc         (alloca)
import           Foreign.Ptr                   (Ptr)
import           Foreign.Storable              (peek)
import           System.IO

import qualified Control.Monad.Terminal.Events as T
import qualified System.Terminal.Ansi.Internal as T

withTerminal :: (MonadIO m, MonadMask m) => Handle -> Handle -> (T.TermEnv -> m a) -> m a
withTerminal hIn hOut action = withTerminalInput $ withTerminalOutput $ do
  mainThreadId <- liftIO myThreadId
  interruptFlag <- liftIO (newTVarIO False)
  eventChan <- liftIO newTChanIO
  screenSize <- liftIO (newTVarIO =<< getScreenSize)
  withResizeMonitoring screenSize eventChan $
    withInputProcessing mainThreadId interruptFlag eventChan $ action $ T.TermEnv {
        T.envInput        = readTChan eventChan
      , T.envInterrupt    = swapTVar interruptFlag False >>= check
      , T.envScreenSize   = readTVar screenSize
      }
  where
    withTerminalInput = bracket
      ( liftIO getConsoleInputModeDesired >>= liftIO . setConsoleInputMode )
      ( liftIO . setConsoleInputMode ) . const
    withTerminalOutput = bracket
      ( liftIO getConsoleOutputModeDesired >>= liftIO . setConsoleOutputMode )
      ( liftIO . setConsoleOutputMode ) . const
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
    withInputProcessing mainThreadId interruptFlag eventChan = bracket
      (liftIO $ async $ processInput mainThreadId interruptFlag eventChan)
      (\a-> liftIO $ withAsync trigger $ const $ cancel a) . const
      where
        trigger = do
          threadDelay 100000 -- Yes, it's a race..
          hPutStr hOut "\ESC[0c"
          hFlush hOut
    withResizeMonitoring screenSize eventChan = bracket
      (liftIO $ async monitor)
      (liftIO . cancel) . const
      where
        monitor = forever $ do
          threadDelay 100000
          new <- getScreenSize
          old <- atomically $ readTVar screenSize
          when (new /= old) $ atomically $ do
            writeTVar screenSize new
            writeTChan eventChan $ T.EvResize new

processInput :: ThreadId -> TVar Bool -> TChan T.Event -> IO ()
processInput mainThreadId interruptFlag chan = flip evalStateT BS.empty $ forever $ do
  event <- T.decodeAnsi
  -- In virtual terminal mode, Windows actually sends Ctrl+C and there is no
  -- way a non-responsive application can be killed from keyboard.
  -- The solution is to catch this specific event and swap an STM interrupt flag.
  -- If the old value is found to be True then it must at least be the second
  -- time the user has pressed Ctrl+C _and_ the application was to busy to
  -- to reset the interrupt flag in the meantime. In this specific case
  -- an asynchronous `E.UserInterrupt` exception is thrown to the main thread
  -- and either terminates the application or at least the current computation.
  when (event == T.EvKey (T.KChar 'C') [T.MCtrl]) $ liftIO $ do
      unhandledInterrupt <- atomically (swapTVar interruptFlag True)
      when unhandledInterrupt (E.throwTo mainThreadId E.UserInterrupt)
  liftIO $ atomically (writeTChan chan $ mapEvent event)
  where
    mapEvent (T.EvKey (T.KChar 'M') [T.MCtrl]) = T.EvKey T.KEnter []
    mapEvent (T.EvKey (T.KChar '\DEL') [])     = T.EvKey (T.KBackspace 1) []
    mapEvent ev                                = ev

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
