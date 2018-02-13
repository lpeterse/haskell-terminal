module System.Terminal
  ( -- * Introduction
    -- ** MonadTerminal
    MonadTerminal(..)
    -- ** AnsiTerminalT
  , AnsiTerminalT ()
  , runAnsiTerminalT

    -- * Printing
    -- ** MonadPrinter
  , MonadPrinter (..)
    -- ** MonadColorPrinter
  , MonadColorPrinter (..)
    -- ** MonadIsolate
  , MonadIsolate (..)

  -- * Cursor Positioning
  -- ** MonadScreen
  , MonadScreen (..)

    -- * Event Handling
    -- ** MonadEvent
  , MonadEvent (..)
  , getEvent
  , tryGetEvent
  ) where

import           System.Terminal.AnsiTerminalT
import           System.Terminal.Class
