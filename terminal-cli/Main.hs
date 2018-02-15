{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified System.Terminal.Repl      as T

main :: IO ()
main = T.runAnsiRepl $ T.getInputLine (T.color T.red "bas $ ") >>= \case
  Nothing     -> T.exit
  Just "quit" -> T.exit
  Just ""     -> pure ()
  Just "size" -> T.getScreenSize >>= \s-> T.putStringLn (show s)
  Just "wait" -> liftIO (threadDelay 3000000)
  Just line   -> T.putStringLn line
