{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Control.Monad.Terminal.Ansi.AnsiTerminal where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad                 (forever, when)
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString               as BS
import           Data.Char
import           Data.Maybe
import qualified Data.Text                     as Text
import           Data.Word

import qualified Control.Monad.Terminal.Events as T
import qualified Control.Monad.Terminal.Modes  as T

data TerminalEnv
  = TerminalEnv
  { envTermType     :: BS.ByteString
  , envInputChars   :: STM Char
  , envInputEvents  :: STM T.Event
  , envInterrupt    :: STM Bool
  , envOutput       :: Text.Text -> STM ()
  , envOutputFlush  :: STM ()
  , envScreenSize   :: STM (Int,Int)
  , envSpecialChars :: Char -> Maybe T.Event
  }
