module Main where

import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Catch    as E
import           Control.Monad.IO.Class
import           Data.Function          (fix)

import           Control.Monad.Terminal
import           System.Terminal.Ansi

main :: IO ()
main = withTerminal $ runAnsiTerminalT $ fix $ \loop-> do
  ev <- waitEvent
  putStringLn (show ev)
  flush
  when (ev == InterruptEvent) (E.throwM UserInterrupt)
  loop
