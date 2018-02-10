{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           System.Environment

import qualified System.Terminal           as T
import qualified System.Terminal.Color     as T
import qualified System.Terminal.Pretty    as T
import qualified System.Terminal.TerminalT as T

main :: IO ()
main = T.runTerminalT foo

foo :: (T.MonadTerminal m, MonadIO m) => m ()
foo = do
  T.putString "Hallo Welt!"
  T.putLn
  T.isolate $ do
    T.setForegroundColor T.red
    T.putStringLn "This should be red!"
  T.putStringLn "This should have default color again!"
  T.cursorDown 6
  T.cursorForward 20
  T.cursorHide
  T.flush
  liftIO $ threadDelay 5000000
  T.cursorShow
  T.flush
  liftIO $ threadDelay 5000000
  T.setUnderline True
  T.putStringLn "FOOBAR"
  T.setUnderline False
  T.setNegative
  T.setNegative
  T.putStringLn "GNURP"
  T.setPositive
  T.setForegroundColor T.red
  T.putStringLn "BLASHDAKSHD"
  liftIO $ threadDelay 10000000
