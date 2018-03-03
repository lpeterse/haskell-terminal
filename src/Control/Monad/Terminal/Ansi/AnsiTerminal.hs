module Control.Monad.Terminal.Ansi.AnsiTerminal where

import           Control.Monad.STM
import           Data.ByteString
import           Data.Text

import           Control.Monad.Terminal.Input

data AnsiTerminal
  = AnsiTerminal
  { ansiTermType    :: ByteString
  , ansiInputChars  :: STM Char
  , ansiInputEvents :: STM Event
  , ansiInterrupt   :: STM ()
  , ansiOutput      :: Text -> STM ()
  , ansiOutputFlush :: STM ()
  , ansiScreenSize  :: STM (Int, Int)
  }
