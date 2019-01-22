{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Terminal
  ( -- * Getting started
    -- ** TerminalT
    TerminalT ()
  , runTerminalT
    -- * Printing & Screen Modification
    -- ** MonadPrinter
  , MonadPrinter (..)
    -- ** MonadPrettyPrinter
  , MonadPrettyPrinter (..)
  , pprint
    -- ** MonadFormatPrinter
  , MonadFormatPrinter (..)
    -- ** MonadColorPrinter
  , MonadColorPrinter (..)
  , dull
  , bright
  , BasicColor (..)
  , Color (..)
  , ColorMode (..)
    -- ** MonadTerminal
  , MonadTerminal (..)
    -- * Event Processing
    -- ** MonadInput
  , MonadInput (..)
    -- *** waitEvent
  , waitEvent
    -- *** waitEventOrElse
  , waitEventOrElse
    -- *** waitInterruptOrElse
  , waitInterruptOrElse
    -- ** Events
  , Event (..)
    -- *** Keys & Modifiers
  , Key (..)
  , Direction (..)
  , Modifiers ()
  , shiftKey
  , ctrlKey
  , altKey
  , metaKey
    -- *** Mouse Events
  , MouseEvent (..)
  , MouseButton (..)
    -- *** Window Events
  , WindowEvent (..)
    -- *** Device Events
  , DeviceEvent (..)
  -- * Low-Level
  -- ** Terminal
  , Terminal (..)
  -- ** Decoding
  , Decoder (..)
  , ansiDecoder
  ) where

import           Control.Monad.Terminal.Decoder
import           Control.Monad.Terminal.Input
import           Control.Monad.Terminal.Monad
import           Control.Monad.Terminal.Printer
import           Control.Monad.Terminal.Terminal
import           Control.Monad.Terminal.TerminalT
