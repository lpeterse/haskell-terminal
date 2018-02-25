module System.Terminal.Ansi
  ( AnsiTerminal (..)
  , AnsiTerminalT ()
  , withTerminal
  , runAnsiTerminalT
  ) where

import           Control.Monad.Terminal.Ansi
import           System.Terminal.Ansi.Platform
