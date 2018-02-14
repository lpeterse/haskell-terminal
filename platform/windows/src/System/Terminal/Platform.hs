module System.Terminal.Platform
  ( TermEnv (..)
  , withTerminal
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception            as E
import           Control.Monad                (forever, void, when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.State
import qualified Data.ByteString              as BS
import           System.IO

import qualified System.Terminal.Ansi         as T
import qualified System.Terminal.Events       as T

data TermEnv
  = TermEnv
  { envInput        :: STM T.Event
  , envInterrupt    :: STM ()
  , envWindowChange :: STM (Int, Int)
  }

withTerminal :: (MonadIO m, MonadMask m) => Handle -> Handle -> (TermEnv -> m a) -> m a
withTerminal hIn hOut action = withTerminalInput $ withTerminalOutput $ do
  mainThreadId <- liftIO myThreadId
  interruptFlag <- liftIO (newTVarIO False)
  eventChan <- liftIO newTChanIO
  bracket
    (liftIO $ async $ processInput mainThreadId interruptFlag eventChan) (liftIO . cancel)

    $ const $ action $ TermEnv {
        envInput        = readTChan eventChan
      , envInterrupt    = swapTVar interruptFlag False >>= check
      , envWindowChange = retry
      }
  where
    withTerminalInput = bracket
      ( liftIO getConsoleInputModeDesired >>= liftIO . setConsoleInputMode )
      ( liftIO . setConsoleInputMode ) . const
    withTerminalOutput = bracket
      ( liftIO getConsoleOutputModeDesired >>= liftIO . setConsoleOutputMode )
      ( liftIO . setConsoleOutputMode ) . const

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

getWindowSize :: IO (Int,Int)
getWindowSize = do
  pure (80, 24)

foreign import ccall unsafe "hs_get_console_input_mode_desired"
  getConsoleInputModeDesired :: IO Int

foreign import ccall unsafe "hs_set_console_input_mode"
  setConsoleInputMode       :: Int -> IO Int

foreign import ccall unsafe "hs_get_console_output_mode_desired"
  getConsoleOutputModeDesired :: IO Int

foreign import ccall unsafe "hs_set_console_output_mode"
  setConsoleOutputMode       :: Int -> IO Int
