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

import qualified Control.Monad.Repl            as R
import qualified Control.Monad.Terminal        as T
import qualified Control.Monad.Terminal.Color  as T
import qualified Control.Monad.Terminal.Events as T
import qualified Control.Monad.Terminal.Pretty as T

import qualified System.Terminal.Ansi          as T

import           Prelude                       hiding (print)

main :: IO ()
main = T.evalAnsiReplT (ini >> repl) 0
  where
    ini = R.setPrompt $ T.putDoc $ T.bold (T.color T.blue "foo") <> "@bar % "

repl :: (R.MonadRepl m, R.ReplState m ~ Int) => m ()
repl = R.readString >>= \case
  Nothing -> R.quit
  Just s -> case s of
    ""     -> pure ()
    "quit" -> R.quit
    "load" -> R.load >>= R.print
    "inc"  -> R.load >>= R.store . succ
    "dec"  -> R.load >>= R.store . pred
    "loop" -> R.print [1..]
    line   -> R.print line
