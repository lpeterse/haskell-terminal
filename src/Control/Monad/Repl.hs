{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Control.Monad.Repl (
    MonadQuit (..)
  , MonadStateful (..)
  , ReplT ()
  , lift
  , runReplT
  , readString
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import qualified Control.Exception           as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Char
import           Data.Function               (fix)
import           Data.Maybe
import qualified Data.Text                   as Text
import           Data.Text.Prettyprint.Doc
import           Data.Typeable
import           Prelude                     hiding (putChar)
import qualified System.IO.Error             as E

import           Control.Monad.Terminal
import qualified Control.Monad.Terminal      as T
import           Control.Monad.Terminal.Ansi

newtype Failure = Failure String
  deriving (Eq, Ord, Show, Typeable)

instance E.Exception Failure

class (Monad m) => MonadQuit m where
  quit :: m a

class (Monad m) => MonadStateful m where
  type State m
  load         :: m (State m)
  store        :: State m -> m ()

newtype ReplT s m a = ReplT { unReplT :: (Either E.SomeException a -> s -> m s) -> s -> m s }
------------------------------------------ continuation ------- exception continuation ------ result ---

runReplT  :: (MonadColorPrinter m) => ReplT s m a -> s -> m s
runReplT (ReplT m) = let foreverM = m
                           (\r s-> handle r >> foreverM s)
                     in  foreverM
  where
    handle (Right _) = pure ()
    handle (Left e) = do
      putDocLn $ fromMaybe renderOtherException tryRenderFailure
      flush
      where
        tryRenderFailure = (\(Failure s)-> template "Failure" $ pretty s) <$> E.fromException e
        renderOtherException = template "Exception" $ pretty (E.displayException e)
        template x y =
             annotate (foreground $ dull Red) $ "*** " <> x <> ": "
          <> annotate (foreground $ bright Red) y

instance (Monad m) => Functor (ReplT s m) where
  fmap f fa = ReplT $ \cont->
    unReplT fa $ \case
      Left  e -> cont (Left e)
      Right a -> cont (Right (f a))

instance (Monad m) => Applicative (ReplT s m) where
  pure a = ReplT $ \cont-> cont (Right a)
  fab <*> fa = ReplT $ \cont->
    unReplT fa $ \case
      Left  e -> cont (Left e)
      Right a -> unReplT fab $ \case
        Left e' -> cont (Left e')
        Right f -> cont (Right (f a))

instance (Monad m) => Monad (ReplT s m) where
  ma >>= k = ReplT $ \cont->
    unReplT ma $ \case
      Left  e -> cont (Left e)
      Right a -> unReplT (k a) cont
  fail e = ReplT $ \cont-> cont (Left $ E.toException $ Failure e)

instance (Monad m) => MonadFail (ReplT s m) where
  fail e = ReplT $ \cont-> cont (Left $ E.toException $ Failure e)

lift :: (MonadCatch m) => m a -> ReplT s m a
lift ma = ReplT $ \cont s->
  try ma >>= \case
    Left  e -> cont (Left e) s
    Right a -> cont (Right a) s

instance (MonadIO m) => MonadIO (ReplT s m) where
  liftIO ma = ReplT $ \cont s->
    liftIO (E.try ma) >>= \case
      Left  e -> cont (Left e) s
      Right a -> cont (Right a) s

instance (MonadThrow m) => MonadThrow (ReplT s m) where
  throwM e = ReplT $ \cont->
    cont $ Left $ E.toException e

instance (MonadThrow m) => MonadCatch (ReplT s m) where
  catch ma ema = ReplT $ \cont->
    unReplT ma $ \case
      Right a -> cont (Right a)
      Left se -> case E.fromException se of
        Nothing -> cont (Left se)
        Just e  -> unReplT (ema e) $ \case
          Right a' -> cont (Right a')
          Left se' -> cont (Left se')

instance (MonadCatch m, MonadPrinter m) => T.MonadPrinter (ReplT s m) where
  putLn = lift T.putLn
  putChar = lift . T.putChar
  putString = lift . T.putString
  putStringLn = lift . T.putStringLn
  putText = lift . T.putText
  putTextLn = lift . T.putTextLn
  flush = lift T.flush
  getLineWidth = lift T.getLineWidth

instance (MonadCatch m, MonadPrettyPrinter m) => MonadPrettyPrinter (ReplT s m) where
  data Annotation (ReplT s m) = Annotation' (T.Annotation m)
  putDoc doc = lift $ T.putDoc (reAnnotate (\(Annotation' ann)-> ann) doc)
  setAnnotation (Annotation' a) = lift (T.setAnnotation a)
  resetAnnotation (Annotation' a) = lift (T.resetAnnotation a)
  resetAnnotations = lift T.resetAnnotations

instance (MonadCatch m, MonadPrettyPrinter m, MonadFormatPrinter m) => MonadFormatPrinter (ReplT s m) where
  bold       = Annotation' T.bold
  italic     = Annotation' T.italic
  underlined = Annotation' T.underlined

instance (MonadCatch m, MonadPrettyPrinter m, T.MonadColorPrinter m) => T.MonadColorPrinter (ReplT s m) where
  inverted     = Annotation' T.inverted
  foreground c = Annotation' (T.foreground c)
  background c = Annotation' (T.background c)

instance (MonadCatch m, MonadScreen m) => T.MonadScreen (ReplT s m) where
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

instance (MonadCatch m, T.MonadInput m) => T.MonadInput (ReplT s m) where
  waitMapInterruptAndEvents = lift . T.waitMapInterruptAndEvents

instance (MonadCatch m, T.MonadTerminal m, T.MonadPrettyPrinter m) => T.MonadTerminal (ReplT s m) where

instance (Monad m) => MonadQuit (ReplT s m) where
  quit = ReplT $ \_ s-> pure s

instance (Monad m) => MonadStateful (ReplT s m) where
  type State (ReplT s m) = s
  load = ReplT $ \cont s-> cont (Right s) s
  store s = ReplT $ \cont _-> cont (Right ()) s

readString :: (MonadQuit m, MonadTerminal m) => Doc (Annotation m) -> m String
readString p = do
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
        readString p
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
