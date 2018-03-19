module System.Terminal
  ( Terminal (..)
  , withTerminal
  ) where

import           Control.Monad.Catch      (MonadMask)
import           Control.Monad.IO.Class   (MonadIO)

import           Control.Monad.Terminal
import qualified System.Terminal.Platform as Platform

-- | (Indirection just for upcoming documentation).
withTerminal :: (MonadIO m, MonadMask m) => (Terminal -> m a) -> m a
withTerminal  = Platform.withTerminal
