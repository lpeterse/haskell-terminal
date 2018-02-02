{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           System.Environment
import           System.IO

import qualified System.Terminal           as T
import qualified System.Terminal.Events    as E
import qualified System.Terminal.Input     as T
import qualified System.Terminal.Modes     as M

main :: IO ()
main = T.withoutEcho $ T.withRawMode $ T.runInputT $ do
  liftIO $ getEnv "TERM" >>= print
  T.askModes >>= liftIO . print
  forever $ do
    ev <- T.decodeAnsi
    liftIO $ putStr (show ev ++ ": ")
    T.isKeyBackspace ev >>= \case
      True -> liftIO $ putStr " isKeyBackspace "
      False -> pure ()
    T.isKeyDelete ev >>= \case
      True -> liftIO $ putStr " isKeyDelete "
      False -> pure ()
    liftIO $ putStrLn ""
