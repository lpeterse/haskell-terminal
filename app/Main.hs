{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           System.Environment

import qualified System.Terminal        as T
import qualified System.Terminal.Input  as T

main :: IO ()
main = T.withoutEcho $ T.withRawMode $ T.runInputT $ do
  liftIO $ lookupEnv "TERM" >>= print
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
