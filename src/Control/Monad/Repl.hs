{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Repl
  ( MonadRepl (..)
  , MonadQuit (..)
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
import           Data.Text.Prettyprint.Doc
import           Data.Typeable
import           Prelude                     hiding (putChar)

import           Control.Monad.Terminal
import qualified Control.Monad.Terminal      as T
import           Control.Monad.Terminal.Ansi

class (Monad m) => MonadQuit m where
  quit :: m a

class (MonadPrinter m, MonadQuit m) => MonadRepl m where
  type ReplState m
  readString   :: Doc (Annotation m) -> m String
  readText     :: Doc (Annotation m) -> m Text.Text
  readText p    = Text.pack <$> readString p
  --load         :: m (ReplState m)
  --store        :: ReplState m -> m ()
  print        :: Show a => a -> m ()

newtype ReplT s m a = ReplT { unReplT ::  ( a -> s -> m s) -> s -> m s }

runReplT  :: (Monad m) => ReplT s m a -> s -> m s
runReplT (ReplT m) = let foreverM = m (const foreverM) in foreverM

instance (Monad m) => Functor (ReplT s m) where
  fmap f fa = ReplT $ \cont->
    unReplT fa (cont . f)

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
  putDoc doc = lift $ T.putDoc (reAnnotate (\(Annotation' ann)-> ann) doc)
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

instance (Monad m) => MonadQuit (ReplT s m) where
  quit = ReplT $ \_ s-> pure s

instance MonadTerminal m => MonadRepl (ReplT s m) where
  type ReplState (ReplT s m) = s
  --setPrompt a = ReplT $ modify (\st-> st { replPrompt = a })
  --load = ReplT $ replUserState <$> get
  --store ust = ReplT $ modify (\st-> st { replUserState = ust })
  print a = T.putStringLn (show a)
  readString = getString

getString :: (MonadQuit m, MonadTerminal m) => Doc (Annotation m) -> m String
getString p = do
  resetAnnotations
  putDoc p
  flush
  withStacks [] []
  where
    withStacks xss yss = T.waitEvent >>= \case
      -- On Ctrl-C most shells just show a new prompt in the next line.
      -- This is probably to not terminate the program after pressing
      -- it several times when trying to interrupt a running computation.
      InterruptEvent -> do
        putLn
        getString p
      -- On Ctrl-D the program is supposed to quit.
      T.KeyEvent (T.KeyChar 'D') mods
        | mods == T.ctrlKey -> do
            putLn
            flush
            quit
      -- On Enter this function returns the entered string to the caller.
      T.KeyEvent T.KeyEnter mods
        | mods == mempty -> do
            putLn
            flush
            pure $ reverse xss ++ yss
      T.KeyEvent T.KeyErase mods
        | mods == mempty -> case xss of
            []     -> withStacks xss yss
            (x:xs) -> do
              cursorBackward 1
              putString yss
              putChar ' '
              cursorBackward (length yss + 1)
              flush
              withStacks xs yss
      T.KeyEvent (T.KLeft 1) mods
        | mods == mempty -> case xss of
            []     -> withStacks xss yss
            (x:xs) -> do
              cursorBackward 1
              flush
              withStacks xs (x:yss)
      T.KeyEvent (T.KRight 1) mods
        | mods == mempty -> case yss of
            []     -> withStacks xss yss
            (y:ys) -> do
              cursorForward 1
              flush
              withStacks (y:xss) ys
      T.KeyEvent (T.KeyChar c) mods
        | mods == mempty && (isPrint c || isSpace c) -> do
            putChar c
            flush
            withStacks (c:xss) yss
        | otherwise ->
            withStacks xss yss
      ev -> do
        --lift $ T.putStringLn (show ev)
        flush
        withStacks xss yss

getPasswordString :: (MonadQuit m, MonadTerminal m) => Doc (Annotation m) -> m String
getPasswordString p = do
  resetAnnotations
  putDoc p
  flush
  withStack []
  where
    withStack xs = waitEvent >>= \case
      -- On Ctrl-C most shells just show a new prompt in the next line.
      -- This is probably to not terminate the program after pressing
      -- it several times when trying to interrupt a running computation.
      InterruptEvent -> do
        putLn
        getPasswordString p
      -- On Ctrl-D the program is supposed to quit.
      KeyEvent (KeyChar 'D') mods | mods == ctrlKey -> do
        putLn
        flush
        quit
      -- On Enter this function returns the entered string to the caller.
      KeyEvent KeyEnter _ -> do
        putLn
        flush
        pure $ reverse xs
      -- On Erase one character is removed from the right.
      KeyEvent KeyErase _ ->
        withStack $! drop 1 xs
      KeyEvent (KeyChar x) mods | mods == mempty ->
        withStack $! x:xs
      _ -> withStack xs
