{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           System.Environment

import qualified System.Terminal           as T
import qualified System.Terminal.Class     as T
import qualified System.Terminal.Color     as T
import qualified System.Terminal.Events    as T
import qualified System.Terminal.Pretty    as T
import qualified System.Terminal.TerminalT as T

main :: IO ()
main = T.withoutEcho $ T.withRawMode $ T.runTerminalT $ do
  liftIO $ lookupEnv "TERM" >>= print
  ev <- T.getEvent
  liftIO $ print ev
  forever $ do
    line <- readLine (T.Colored T.red "bas $ ")
    T.putStringLn line

readLine :: (T.MonadTerminal m) => T.Doc -> m String
readLine prompt = do
  T.putDoc prompt
  T.setDefault
  T.flush
  withStack []
  where
    withStack cs = T.getEvent >>= \case
      T.EvKey (T.KChar [] '\n') [] -> T.putLn >> T.flush >> pure (reverse cs)
      T.EvKey (T.KChar [] c) []
        | isPrint c || isSpace c -> T.putChar c >> T.flush >> withStack (c:cs)
        | otherwise              -> withStack cs
      _ -> withStack cs
