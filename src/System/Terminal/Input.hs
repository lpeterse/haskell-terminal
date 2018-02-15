{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module System.Terminal.Input
  ( MonadInput (..)
  , InputT ()
  , runInputT
  ) where

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

class MonadInput m where
  getInputLine :: T.TermDoc -> m (Maybe String)

newtype InputT m a = InputT (StateT () m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance (T.MonadPrinter m) => T.MonadPrinter (InputT m) where
  putLn = lift T.putLn
  putChar = lift . T.putChar
  putString = lift . T.putString
  putStringLn = lift . T.putStringLn
  putText = lift . T.putText
  putTextLn = lift . T.putTextLn
  flush = lift T.flush

instance (T.MonadIsolate m) => T.MonadIsolate (InputT m) where
  isolate (InputT sma) = InputT $ do
    st <- get
    (a,st') <- lift $ T.isolate $ runStateT sma st
    put st'
    pure a

instance (T.MonadColorPrinter m) => T.MonadColorPrinter (InputT m) where
  setDefault = lift T.setDefault
  setForegroundColor = lift . T.setForegroundColor
  setBackgroundColor = lift . T.setBackgroundColor
  setUnderline = lift . T.setUnderline
  setNegative = lift . T.setNegative

instance (T.MonadScreen m) => T.MonadScreen (InputT m) where
  clear = lift T.clear
  cursorUp = lift . T.cursorUp
  cursorDown = lift . T.cursorDown
  cursorForward = lift . T.cursorForward
  cursorBackward = lift . T.cursorBackward
  cursorPosition x y = lift $ T.cursorPosition x y
  cursorVisible = lift . T.cursorVisible
  getScreenSize = lift T.getScreenSize

instance (T.MonadEvent m) => T.MonadEvent (InputT m) where
  withEventSTM = lift . T.withEventSTM

instance (T.MonadTerminal m) => T.MonadTerminal (InputT m) where

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
