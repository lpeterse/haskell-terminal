module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           System.Exit

import qualified System.Terminal        as T
import qualified System.Terminal.Events as T

main :: IO ()
main = T.runAnsiTerminalT $ forever $ do
  ev <- T.getEvent
  T.putStringLn (show ev)
  T.flush
  when (ev == T.EvKey (T.KChar 'C') [T.MCtrl]) (liftIO exitSuccess)
