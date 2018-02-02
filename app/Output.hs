{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           System.Environment

import qualified System.Terminal         as T
import qualified System.Terminal.Input   as T
import qualified System.Terminal.Output  as T
import qualified System.Terminal.Windows as T

main :: IO ()
main = T.withoutEcho $ T.withRawMode $ T.runTerminalT $ do
  T.putString "Hallo Welt!"
  T.nl
  T.setForegroundColor (T.Color T.Red True)
  T.putStringLn "This should be red!"
  T.setForegroundColor T.ColorDefault
  T.putStringLn "This should have default color again!"
  T.setBackgroundColor (T.Color T.Cyan False)
  T.putString "ABC"
  T.cr
  T.putString "DEF"
  T.nl
  T.putString "HIJ"
