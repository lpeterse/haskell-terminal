{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module System.Terminal.Repl
  ( MonadRepl (..)
  , ReplT ()
  , runReplT
  , runAnsiReplT
  , runAnsiReplIO
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
import qualified Data.Text                  as Text
import qualified Data.Text.Prettyprint.Doc  as PP
import           Data.Typeable
import           System.Environment
import qualified System.Exit                as SE

import qualified System.Terminal            as T
import qualified System.Terminal.Color      as T
import qualified System.Terminal.Events     as T
import qualified System.Terminal.Pretty     as T

class Pretty a where
  pretty :: a -> PP.Doc ann

class T.MonadTerminal m => MonadRepl m where
  type ReplState m
  setPrompt    :: Pretty a => a -> m ()
  readString   :: m String
  readText     :: m Text.Text
  readText      = Text.pack <$> readString
  load         :: m (ReplState m)
  store        :: ReplState m -> m ()
  print        :: Show a => a -> m ()
  pprint       :: Pretty a => a -> m ()
  exit         :: m ()
  exitWith     :: SE.ExitCode -> m ()

newtype ReplT s m a = ReplT (StateT s (StateT (Maybe SE.ExitCode) m) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (ReplT s) where
  lift = ReplT . lift . lift

instance (T.MonadPrinter m) => T.MonadPrinter (ReplT s m) where
  putLn = lift T.putLn
  putChar = lift . T.putChar
  putString = lift . T.putString
  putStringLn = lift . T.putStringLn
  putText = lift . T.putText
  putTextLn = lift . T.putTextLn
  flush = lift T.flush

instance (T.MonadIsolate m) => T.MonadIsolate (ReplT s m) where
  isolate (ReplT sma) = ReplT $ do
    s1 <- get
    s2 <- lift get
    ((a,s1'),s2') <- lift $ lift $ T.isolate $ runStateT (runStateT sma s1) s2
    put s1'
    lift (put s2')
    pure a

instance (T.MonadColorPrinter m) => T.MonadColorPrinter (ReplT s m) where
  setDefault = lift T.setDefault
  setForegroundColor = lift . T.setForegroundColor
  setBackgroundColor = lift . T.setBackgroundColor
  setUnderline = lift . T.setUnderline
  setNegative = lift . T.setNegative

instance (T.MonadScreen m) => T.MonadScreen (ReplT s m) where
  clear = lift T.clear
  cursorUp = lift . T.cursorUp
  cursorDown = lift . T.cursorDown
  cursorForward = lift . T.cursorForward
  cursorBackward = lift . T.cursorBackward
  cursorPosition x y = lift $ T.cursorPosition x y
  cursorVisible = lift . T.cursorVisible
  getScreenSize = lift T.getScreenSize

instance (T.MonadEvent m) => T.MonadEvent (ReplT s m) where
  withEventSTM = lift . T.withEventSTM

instance (T.MonadTerminal m) => T.MonadTerminal (ReplT s m) where

type AnsiReplT s m = ReplT s (T.AnsiTerminalT m)
type AnsiReplIO s = AnsiReplT s IO

runReplT :: (T.MonadTerminal m) => s -> ReplT s m () -> m (SE.ExitCode, s)
runReplT s (ReplT ma) = evalStateT (runStateT loop s) Nothing
  where
    loop = ma >> lift get >>= \case
      Nothing -> loop
      Just code -> pure code

runAnsiReplT :: s -> AnsiReplT s IO () -> IO (SE.ExitCode, s)
runAnsiReplT s ma = T.runAnsiTerminalT $ runReplT s ma

runAnsiReplIO :: s -> AnsiReplIO s () -> IO ()
runAnsiReplIO s ma = runAnsiReplT s ma >>= SE.exitWith . fst

instance T.MonadTerminal m => MonadRepl (ReplT s m) where
  type ReplState (ReplT s m) = s
  setPrompt a = pure ()
  load = ReplT $ get
  store = ReplT . put
  print a = T.putStringLn (show a)
  pprint a = T.putDoc (pretty a)
  exit = ReplT $ lift $ put (Just SE.ExitSuccess)
  exitWith = ReplT . lift . put . Just
  readString = do
    lift $ T.putString "> "
    lift $ T.setDefault
    lift $ T.flush
    withStacks [] []
    where
      withStacks xss yss = lift (T.withEventSTM id) >>= \case
        T.EvKey (T.KChar 'C') [T.MCtrl] -> do
          lift $ T.putLn
          lift $ T.flush
          readString
        T.EvKey (T.KChar 'D') [T.MCtrl] -> do
          lift $ T.putLn
          lift $ T.flush
          error "FIXME"
        T.EvKey T.KEnter [] -> do
          lift $ T.putLn
          lift $ T.flush
          pure $ reverse xss ++ yss
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
