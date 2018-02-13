module System.Terminal.Platform
  ( withTerminal
  , withHookedInterruptSignal
  , gatherInputEvents
  ) where

import           Control.Concurrent
import qualified Control.Concurrent.Async    as A
import           Control.Concurrent.STM.TVar
import           Control.Exception
import qualified Control.Exception           as E
import           Control.Monad               (forever, when)
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import qualified Data.ByteString             as BS
import qualified System.IO                   as IO
import qualified System.Posix.Signals        as Posix

import qualified System.Terminal.Ansi        as T
import qualified System.Terminal.Events      as T

withTerminal :: IO a -> IO a
withTerminal = withRawMode . withoutEcho

withHookedInterruptSignal :: (STM () -> IO a) -> IO a
withHookedInterruptSignal action = do
  sig <- newTVarIO False
  bracket
    (flip (Posix.installHandler Posix.sigINT) Nothing  $ Posix.Catch $ atomically $ writeTVar sig True)
    (flip (Posix.installHandler Posix.sigINT) Nothing) $ const $ action (readTVar sig >>= check >> writeTVar sig False)

withoutEcho :: IO a -> IO a
withoutEcho = bracket
  (IO.hGetEcho IO.stdin >>= \x-> IO.hSetEcho IO.stdin False >> pure x)
  (IO.hSetEcho IO.stdin) . const

withRawMode :: IO a -> IO a
withRawMode = bracket
  (IO.hGetBuffering IO.stdin >>= \b-> IO.hSetBuffering IO.stdin IO.NoBuffering >> pure b)
  (IO.hSetBuffering IO.stdin) . const

gatherInputEvents :: (T.Event -> STM ()) -> IO ()
gatherInputEvents push = flip evalStateT BS.empty $ forever $
  liftIO . atomically . push =<< mapEvent <$> T.decodeAnsi
  where
    mapEvent (T.EvKey (T.KChar '\DEL') [])     = T.EvKey (T.KBackspace 1) []
    mapEvent (T.EvKey (T.KChar 'J') [T.MCtrl]) = T.EvKey T.KEnter []
    mapEvent ev                                = ev
