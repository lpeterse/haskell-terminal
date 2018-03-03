module Control.Monad.Terminal.Terminal where

import           Control.Monad.STM
import           Data.ByteString
import           Data.Text

import           Control.Monad.Terminal.Input

data Terminal
  = Terminal
  { termType        :: ByteString
  , termInputEvents :: STM Event
  , termInterrupt   :: STM ()
  , termOutput      :: Text -> STM ()
  , termOutputFlush :: STM ()
  , termScreenSize  :: STM (Int, Int)
  }
