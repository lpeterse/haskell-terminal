module Control.Monad.Terminal.Ansi.AnsiTerminal where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Data.ByteString
import           Data.Char
import           Data.Text

import           Control.Monad.Terminal.Events

data AnsiTerminal
  = AnsiTerminal
  { ansiTermType     :: ByteString
  , ansiInputChars   :: STM Char
  , ansiInputEvents  :: STM Event
  , ansiInterrupt    :: STM Bool
  , ansiOutput       :: Text -> STM ()
  , ansiOutputFlush  :: STM ()
  , ansiScreenSize   :: STM (Int, Int)
  , ansiSpecialChars :: Char -> Maybe Event
  }
