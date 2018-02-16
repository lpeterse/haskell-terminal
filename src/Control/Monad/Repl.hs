{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Control.Monad.Repl
  ( MonadRepl (..)
  , ReplT ()
  , execReplT
  , execAnsiReplT
  , evalAnsiReplT
  , read
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Function                 (fix)
import qualified Data.Text                     as Text
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Typeable

import qualified Control.Monad.Terminal        as T
import qualified Control.Monad.Terminal.Color  as T
import qualified Control.Monad.Terminal.Events as T
import qualified Control.Monad.Terminal.Pretty as T

import qualified System.Terminal.AnsiTerminalT as T

import           Prelude                       hiding (read)

class Pretty a where
  pretty :: a -> PP.Doc ann

class T.MonadPrinter m => MonadRepl m where
  type ReplState m
  setPrompt    :: m () -> m ()
  readString   :: m String
  readText     :: m Text.Text
  readText      = Text.pack <$> readString
  load         :: m (ReplState m)
  store        :: ReplState m -> m ()
  print        :: Show a => a -> m ()
  pprint       :: Pretty a => a -> m ()
  quit         :: m ()

read :: (MonadRepl m, Read a) => m a
read = do
  s <- readString
  case reads s of
    []        -> fail "no parse"
    ((x,_):_) -> pure x

newtype ReplT s m a = ReplT (StateT (ReplTState s m) m a)
  deriving (Functor, Applicative, Monad, MonadIO)

data ReplTState s m
  = ReplTState
  { replQuit      :: Bool
  , replPrompt    :: ReplT s m ()
  , replUserState :: s
  }

replTStateDefault :: T.MonadPrinter m => s -> ReplTState s m
replTStateDefault s = ReplTState {
    replQuit   = False
  , replPrompt = T.putString "> "
  , replUserState = s
  }

instance MonadTrans (ReplT s) where
  lift = ReplT . lift

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
    (a,s2) <- lift $ T.isolate $ runStateT sma s1
    put s2
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

execReplT :: (T.MonadTerminal m, T.MonadPrinter m) => ReplT s m () -> s -> m s
execReplT (ReplT ma) s = replUserState <$> execStateT loop (replTStateDefault s)
  where
    loop = ma >> (replQuit <$> get) >>= \case
      False -> loop
      True  -> pure ()

execAnsiReplT :: AnsiReplT s IO () -> s -> IO s
execAnsiReplT ma = T.runAnsiTerminalT . execReplT ma

evalAnsiReplT :: AnsiReplT s IO () -> s -> IO ()
evalAnsiReplT ma = void . execAnsiReplT ma

instance T.MonadTerminal m => MonadRepl (ReplT s m) where
  type ReplState (ReplT s m) = s
  setPrompt a = ReplT $ modify (\st-> st { replPrompt = a })
  load = ReplT $ replUserState <$> get
  store ust = ReplT $ modify (\st-> st { replUserState = ust })
  print a = T.putStringLn (show a)
  pprint a = T.putDoc (pretty a)
  quit = ReplT $ modify (\st-> st { replQuit = True })
  readString = do
    prompt <- ReplT $ replPrompt <$> get
    prompt
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
