{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Control.Monad.Repl
  ( MonadRepl (..)
  , ReplT ()
  , runReplT
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import qualified Control.Exception           as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Function               (fix)
import qualified Data.Text                   as Text
import qualified Data.Text.Prettyprint.Doc   as PP
import           Data.Typeable

import qualified Control.Monad.Terminal      as T

import           Prelude                     hiding (read)

class T.MonadPrinter m => MonadRepl m where
  type ReplState m
  --setPrompt    :: m () -> m ()
  readString   :: m (Maybe String)
  readText     :: m (Maybe Text.Text)
  readText      = (Text.pack <$>) <$> readString
  --load         :: m (ReplState m)
  --store        :: ReplState m -> m ()
  print        :: Show a => a -> m ()
  quit         :: m a

newtype ReplT s m a = ReplT { unReplT ::  ( a -> s -> m s) -> s -> m s }

runReplT  :: (Monad m) => ReplT s m a -> s -> m s
runReplT m = unReplT m (const pure)

instance (Monad m) => Functor (ReplT s m) where
  fmap f fa = ReplT $ \cont->
    unReplT fa (\a1-> cont (f a1))

instance (Monad m) => Applicative (ReplT s m) where
  pure a = ReplT $ \cont s-> cont a s
  fab <*> fa = ReplT $ \cont->
    unReplT fa (\a-> unReplT fab (\f-> cont (f a)))

instance (Monad m) => Monad (ReplT s m) where
  ma >>= k = ReplT $ \cont->
    unReplT ma (\a-> unReplT (k a) cont)

instance MonadTrans (ReplT s) where
  lift ma = ReplT $ \cont s->
    ma >>= \a-> cont a s

instance (MonadIO m) => MonadIO (ReplT s m) where
  liftIO ma = ReplT $ \cont s->
    liftIO ma >>= \a-> cont a s

instance (T.MonadPrinter m) => T.MonadPrinter (ReplT s m) where
  putLn = lift T.putLn
  putChar = lift . T.putChar
  putString = lift . T.putString
  putStringLn = lift . T.putStringLn
  putText = lift . T.putText
  putTextLn = lift . T.putTextLn
  flush = lift T.flush
  getLineWidth = lift T.getLineWidth

instance (T.MonadPrettyPrinter m) => T.MonadPrettyPrinter (ReplT s m) where
  data Annotation (ReplT s m) = Annotation' (T.Annotation m)
  putDoc doc = lift $ T.putDoc (PP.reAnnotate (\(Annotation' ann)-> ann) doc)
  setAnnotation (Annotation' a) = lift (T.setAnnotation a)
  resetAnnotation (Annotation' a) = lift (T.resetAnnotation a)
  resetAnnotations = lift T.resetAnnotations

instance (T.MonadPrettyPrinter m, T.MonadFormatPrinter m) => T.MonadFormatPrinter (ReplT s m) where
  bold       = Annotation' T.bold
  italic     = Annotation' T.italic
  underlined = Annotation' T.underlined

instance (T.MonadPrettyPrinter m, T.MonadColorPrinter m) => T.MonadColorPrinter (ReplT s m) where
  inverted     = Annotation' T.inverted
  foreground c = Annotation' (T.foreground c)
  background c = Annotation' (T.background c)

instance (T.MonadScreen m) => T.MonadScreen (ReplT s m) where
  clear = lift T.clear
  putCr = lift T.putCr
  cursorUp = lift . T.cursorUp
  cursorDown = lift . T.cursorDown
  cursorForward = lift . T.cursorForward
  cursorBackward = lift . T.cursorBackward
  cursorPosition x y = lift $ T.cursorPosition x y
  cursorVisible = lift . T.cursorVisible
  getScreenSize = lift T.getScreenSize
  getCursorPosition = lift T.getCursorPosition

instance (T.MonadInput m) => T.MonadInput (ReplT s m) where
  waitMapInterruptAndEvents = lift . T.waitMapInterruptAndEvents

instance (T.MonadTerminal m, T.MonadPrettyPrinter m) => T.MonadTerminal (ReplT s m) where

instance T.MonadTerminal m => MonadRepl (ReplT s m) where
  type ReplState (ReplT s m) = s
  quit = ReplT $ \_ s-> pure s
  --setPrompt a = ReplT $ modify (\st-> st { replPrompt = a })
  --load = ReplT $ replUserState <$> get
  --store ust = ReplT $ modify (\st-> st { replUserState = ust })
  print a = T.putStringLn (show a)
  readString = do
    lift $ T.resetAnnotations
    lift $ T.flush
    withStacks [] []
    where
      withStacks xss yss = T.waitEvent >>= \case
        T.EvKey (T.KChar 'C') [T.MCtrl] -> do
          lift $ T.putLn
          lift $ T.flush
          readString
        T.EvKey (T.KChar 'D') [T.MCtrl] -> do
          lift $ T.putLn
          lift $ T.flush
          pure Nothing
        T.EvKey T.KEnter [] -> do
          lift $ T.putLn
          lift $ T.flush
          pure $ Just $ reverse xss ++ yss
        T.EvKey T.KErase [] -> case xss of
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
          --lift $ T.putStringLn (show ev)
          lift $ T.flush
          withStacks xss yss

