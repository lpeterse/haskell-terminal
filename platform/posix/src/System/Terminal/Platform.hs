module System.Terminal.Platform
  ( withTerminal ) where

import           Control.Monad          (void)
import           Control.Monad.Catch
import           Control.Monad.IO.Class

withTerminal :: (MonadIO m, MonadMask m) => m a -> m a
withTerminal = id
