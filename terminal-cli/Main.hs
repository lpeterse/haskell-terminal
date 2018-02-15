{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Char
import           Data.Function             (fix)
import           System.Environment

import qualified System.Terminal           as T
import qualified System.Terminal.Color     as T
import qualified System.Terminal.Events    as T
import qualified System.Terminal.Input     as T
import qualified System.Terminal.Pretty    as T

main :: IO ()
main = T.runAnsiTerminalT $ T.runInputT $ fix $ \continue->
  T.getInputLine (T.color T.red "bas $ ") >>= \case
    Nothing     -> pure ()
    Just ""     -> continue
    Just "quit" -> pure ()
    Just "size" -> T.getScreenSize >>= \s-> T.putStringLn (show s) >> continue
    Just "wait" -> liftIO (threadDelay 3000000) >> continue
    Just line   -> T.putStringLn line >> continue
