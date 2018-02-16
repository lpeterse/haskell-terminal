{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Char
import           System.Environment

import qualified System.Terminal           as T
import qualified System.Terminal.Color     as T
import qualified System.Terminal.Events    as T
import qualified System.Terminal.Pretty    as T
import qualified System.Terminal.Repl      as R

import           Prelude                   hiding (print)

main :: IO ()
main = R.runAnsiReplIO 0 repl

repl :: (R.MonadRepl m, R.ReplState m ~ Int) => m ()
repl = R.readString >>= \case
  "exit" -> R.exit
  ""     -> pure ()
  "load" -> R.load >>= R.print
  "inc"  -> R.load >>= R.store . succ
  "dec"  -> R.load >>= R.store . pred
  line   -> R.print line
