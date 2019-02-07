{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes   #-}
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
  , EraseMode (..)
    -- ** MonadTerminal
  , MonadTerminal
    -- * Event Processing
  , MonadInput (..)
    -- *** waitEvent
  , waitEvent
  , checkInterrupt
    -- ** Events
  , Event (..)
  , Interrupt (..)
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
    -- * Low-Level / Misc
  , Row, Rows, Col, Cols
    -- ** Terminal
  , Terminal (..)
  , Command (..)
  , Decoder (..)
  , defaultDecoder
  , defaultEncode
    -- ** LocalTerminal
  , System.Terminal.Platform.LocalTerminal ()
    -- ** VirtualTerminal (for testing)
  , VirtualTerminal (..)
  , VirtualTerminalSettings (..)
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import           System.Terminal.Decoder
import           System.Terminal.Encoder
import           System.Terminal.MonadInput
import           System.Terminal.MonadPrinter
import           System.Terminal.MonadScreen
import           System.Terminal.MonadTerminal
import           System.Terminal.Pretty
import           System.Terminal.Terminal
import           System.Terminal.TerminalT
import qualified System.Terminal.Platform
import           System.Terminal.Virtual

-- | Run the given handler with the locally connected terminal (`System.IO.stdin` / `System.IO.stdout`).
--
-- @
-- import System.Terminal
--
-- main :: IO ()
-- main = withTerminal $ `runTerminalT` do
--     `putTextLn` "Hello world!"
--     `flush`
-- @
withTerminal :: (MonadIO m, MonadMask m) => (System.Terminal.Platform.LocalTerminal -> m a) -> m a
withTerminal = System.Terminal.Platform.withTerminal
