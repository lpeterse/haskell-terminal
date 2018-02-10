module System.Terminal
  ( -- * Introduction
    MonadTerminal(..)

    -- * Printing
    -- ** MonadPrinter
  , MonadPrinter (..)
    -- ** MonadColorPrinter
  , MonadColorPrinter (..)
    -- ** MonadIsolate
  , MonadIsolate (..)

  -- * MonadScreen
  , MonadScreen (..)

    -- * Input Events
    -- ** MonadEvent
  , MonadEvent (..)
  , getEvent
  , tryGetEvent
  ) where

import           System.Terminal.Class
