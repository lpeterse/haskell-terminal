module Main where

import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Catch           as E
import           Control.Monad.IO.Class
import           Data.Function                 (fix)

import qualified Control.Monad.Terminal        as T
import qualified Control.Monad.Terminal.Ansi   as T
import qualified Control.Monad.Terminal.Events as T

import qualified System.Terminal.Ansi          as T

main :: IO ()
main = T.withTerminal $ T.runAnsiTerminalT $ fix $ \loop-> do
  ev <- T.getEvent
  T.putStringLn (show ev)
  T.flush
  when (ev == T.EvKey (T.KChar 'C') [T.MCtrl]) (E.throwM UserInterrupt)
  when (ev /= T.EvKey (T.KChar 'D') [T.MCtrl]) loop
