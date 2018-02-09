{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           System.Environment

import qualified System.Terminal           as T
import qualified System.Terminal.Class     as T
import qualified System.Terminal.Pretty    as T
import qualified System.Terminal.TerminalT as T

main :: IO ()
main = T.withoutEcho $ T.withRawMode $ T.runTerminalT $ foo

foo :: (T.MonadScreen m, MonadIO m) => m ()
foo = do
  T.putString "Hallo Welt!"
  T.nl
  --T.setForegroundColor (T.Color T.Red True)
  T.putStringLn "This should be red!"
  --T.setForegroundColor T.ColorDefault
  T.putStringLn "This should have default color again!"
  --T.setBackgroundColor (T.Color T.Cyan False)
  T.putString "ABC"
  T.cr
  T.putString "DEF"
  T.nl
  T.putString "HIJ"
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
  T.setForegroundColor (T.Color T.Red True)
  T.putStringLn "BLASHDAKSHD"
  liftIO $ threadDelay 10000000
