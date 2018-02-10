module System.Terminal
  ( -- * Introduction
    -- ** MonadTerminal
    MonadTerminal(..)

    -- * Printing
    -- ** MonadPrinter
  , MonadPrinter (..)
    -- ** MonadColorPrinter
  , MonadColorPrinter (..)
    -- ** MonadIsolate
  , MonadIsolate (..)
    -- ** Pretty Printing
  , putDoc

  -- * MonadScreen
  , MonadScreen (..)

    -- * Input Events
    -- ** MonadEvent
  , MonadEvent (..)
  , getEvent
  , tryGetEvent

  ) where

import           System.Terminal.Class
