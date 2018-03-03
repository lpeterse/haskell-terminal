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
  when (ev == KeyEvent (KeyChar 'C') ctrlKey) (E.throwM UserInterrupt)
  when (ev /= KeyEvent (KeyChar 'D') ctrlKey) loop
