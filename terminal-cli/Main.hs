{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Function              (fix)
import           System.Environment

import qualified System.Terminal            as T
import qualified System.Terminal.Color      as T
import qualified System.Terminal.Events     as T
import qualified System.Terminal.Pretty     as T

main :: IO ()
main = T.runAnsiTerminalT $ runInputT $ fix $ \continue->
  getInputLine (T.color T.red "bas $ ") >>= \case
    Nothing     -> pure ()
    Just "quit" -> pure ()
    Just "size" -> lift T.getScreenSize >>= \s-> lift (T.putStringLn $ show s) >> continue
    Just ""     -> continue
    Just "wait" -> liftIO (threadDelay 3000000) >> continue
    Just line   -> lift (T.putStringLn line) >> continue

class MonadInput m where
  getInputLine :: T.TermDoc -> m (Maybe String)

newtype InputT m a = InputT (StateT () m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

runInputT :: T.MonadTerminal m => InputT m a -> m a
runInputT (InputT ma) = evalStateT ma ()

instance T.MonadTerminal m => MonadInput (InputT m) where
  getInputLine prompt = do
    lift $ T.putDoc prompt
    lift $ T.setDefault
    lift $ T.flush
    withStacks [] []
    where
      withStacks xss yss = lift (T.withEventSTM id) >>= \case
        T.EvKey (T.KChar 'C') [T.MCtrl] -> do
          lift $ T.putLn
          lift $ T.flush
          getInputLine prompt
        T.EvKey (T.KChar 'D') [T.MCtrl] -> do
          lift $ T.putLn
          lift $ T.flush
          pure Nothing
        T.EvKey T.KEnter [] -> do
          lift $ T.putLn
          lift $ T.flush
          pure (Just $ reverse xss ++ yss)
        T.EvKey (T.KBackspace 1) [] -> case xss of
          []     -> withStacks xss yss
          (x:xs) -> do
            lift $ T.cursorBackward 1
            lift $ T.putString yss
            lift $ T.putChar ' '
            lift $ T.cursorBackward (length yss + 1)
            lift $ T.flush
            withStacks xs yss
        T.EvKey (T.KLeft 1) [] -> case xss of
          []     -> withStacks xss yss
          (x:xs) -> do
            lift $ T.cursorBackward 1
            lift $ T.flush
            withStacks xs (x:yss)
        T.EvKey (T.KRight 1) [] -> case yss of
          []     -> withStacks xss yss
          (y:ys) -> do
            lift $ T.cursorForward 1
            lift $ T.flush
            withStacks (y:xss) ys
        T.EvKey (T.KChar c) []
          | isPrint c || isSpace c -> do
              lift $ T.putChar c
              lift $ T.flush
              withStacks (c:xss) yss
          | otherwise -> do
            withStacks xss yss
        ev -> do
          lift $ T.putStringLn (show ev)
          lift $ T.flush
          withStacks xss yss
