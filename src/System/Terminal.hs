module System.Terminal
  ( -- * Getting started
    -- ** withTerminal
    withTerminal
    -- ** TerminalT
  , runTerminalT
  , TerminalT ()
    -- * Printing & Screen Modification
    -- ** MonadPrinter
  , MonadPrinter (..)
    -- *** MonadMarkupPrinter
  , MonadMarkupPrinter (..)
    -- *** MonadFormattingPrinter
  , MonadFormattingPrinter (..)
    -- *** MonadColorPrinter
  , MonadColorPrinter (..)
    -- *** Pretty Printing
  , putDoc
  , putDocLn
  , putPretty
  , putPrettyLn
  , putSimpleDocStream
    -- ** MonadScreen
  , MonadScreen (..)
  , Size (..)
  , Position (..)
  , EraseMode (..)
    -- ** MonadTerminal
  , MonadTerminal
    -- * Event Processing
  , MonadInput (..)
    -- *** awaitEvent
  , awaitEvent
    -- *** checkInterrupt
  , checkInterrupt
    -- ** Events
  , Event (..)
  , Interrupt (..)
    -- *** Keys & Modifiers
  , Key (..)
  , Modifiers ()
  , shiftKey
  , ctrlKey
  , altKey
  , metaKey
  , Direction (..)
    -- *** Mouse Events
  , MouseEvent (..)
  , MouseButton (..)
    -- *** Window Events
  , WindowEvent (..)
    -- *** Device Events
  , DeviceEvent (..)
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import           System.Terminal.MonadInput
import           System.Terminal.MonadPrinter
import           System.Terminal.MonadScreen
import           System.Terminal.MonadTerminal
import           System.Terminal.Pretty
import           System.Terminal.TerminalT
import qualified System.Terminal.Platform

-- | Run the given handler with the locally connected terminal (`System.IO.stdin` / `System.IO.stdout`).
--
-- @
-- import System.Terminal
--
-- main :: IO ()
-- main = withTerminal $ `runTerminalT` do
--     `putTextLn` "Hello there, please press a button!"
--     `flush`
--     ev <- `waitEvent`
--     `putStringLn` $ "Event was " ++ show ev
--     `flush`
-- @
withTerminal :: (MonadIO m, MonadMask m) => (System.Terminal.Platform.LocalTerminal -> m a) -> m a
withTerminal = System.Terminal.Platform.withTerminal
