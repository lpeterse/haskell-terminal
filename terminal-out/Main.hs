{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           System.Environment

import qualified System.Terminal        as T
import qualified System.Terminal.Color  as T
import qualified System.Terminal.Pretty as T

main :: IO ()
main = T.runAnsiTerminalT foo

foo :: (T.MonadTerminal m, MonadIO m) => m ()
foo = do
  T.putString "Hallo Welt!"
  T.putLn
  T.putDocLn $ T.color T.red "This should be red!"
  T.putStringLn "This should have default color again!"
  T.cursorDown 6
  T.cursorForward 20
  T.cursorVisible False
  T.flush
  liftIO $ threadDelay 5000000
  T.cursorVisible True
  T.flush
  liftIO $ threadDelay 5000000
  T.setUnderline True
  T.putStringLn "FOOBAR"
  T.setUnderline False
  T.setNegative True
  T.putStringLn "GNURP"
  T.setNegative False
  T.setForegroundColor T.red
  T.putStringLn "BLASHDAKSHD"
  liftIO $ threadDelay 10000000
