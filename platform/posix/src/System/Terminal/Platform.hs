module System.Terminal.Platform
  ( withTerminal
  , withHookedInterruptSignal
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad.STM
import qualified System.Posix.Signals        as Posix

withTerminal :: IO a -> IO a
withTerminal = id

withHookedInterruptSignal :: (STM () -> IO a) -> IO a
withHookedInterruptSignal action = do
  sig <- newTVarIO False
  bracket
    (flip (Posix.installHandler Posix.sigINT) Nothing  $ Posix.Catch $ atomically $ writeTVar sig True)
    (flip (Posix.installHandler Posix.sigINT) Nothing) $ const $ action (readTVar sig >>= check >> writeTVar sig False)
