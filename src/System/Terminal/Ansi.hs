module System.Terminal.Ansi
  ( AnsiTerminal (..)
  , AnsiTerminalT ()
  , withTerminal
  , runAnsiTerminalT
  ) where

import           Control.Monad.Catch           (MonadMask)
import           Control.Monad.IO.Class        (MonadIO)

import           Control.Monad.Terminal.Ansi
import qualified System.Terminal.Ansi.Platform as Platform

-- | (Indirection just for upcoming documentation).
withTerminal :: (MonadIO m, MonadMask m) => (AnsiTerminal -> m a) -> m a
withTerminal  = Platform.withTerminal
