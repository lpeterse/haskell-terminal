{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Char
import           Data.Monoid
import           System.Environment

import qualified Control.Monad.Terminal        as T
import qualified Control.Monad.Terminal.Color  as T
import qualified Control.Monad.Terminal.Events as T
import qualified Control.Monad.Terminal.Pretty as T

import qualified Control.Monad.Repl            as R

import           Prelude                       hiding (print)

main :: IO ()
main = R.evalAnsiReplT (ini >> repl) 0
  where
    ini = R.setPrompt $ T.putDoc $ T.color T.cyan "REPL " <> T.color T.red "> "

repl :: (R.MonadRepl m, R.ReplState m ~ Int) => m ()
repl = R.readString >>= \case
  "quit"  -> R.quit
  ""      -> pure ()
  "load"  -> R.load >>= R.print
  "store" -> R.read >>= R.store
  "inc"   -> R.load >>= R.store . succ
  "dec"   -> R.load >>= R.store . pred
  line    -> R.print line
