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
  , getPasswordString
  ) where

import qualified Control.Exception         as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Data.Char
import           Data.IORef
import           Data.Maybe
import           Data.Text.Prettyprint.Doc
import           Data.Typeable
import           Prelude                   hiding (putChar)

import           Control.Monad.Terminal
import qualified Control.Monad.Terminal    as T

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
---------------------------------------------------- continuation ---------------- result ---

runReplT  :: (MonadColorPrinter m) => ReplT s m a -> s -> m s
runReplT (ReplT m) = let foreverM = m (\r s-> processResult r >> foreverM s)
                     in  foreverM
  where
    processResult (Right _) = pure ()
    processResult (Left e) = do
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
        Just  e -> unReplT (ema e) cont
        Nothing -> cont (Left se)

-- | This instance uses a trick involving an IORef in order to
--   intermediately terminate and evaluate the ReplT transformer
--   and then eventually call the continuation or not.
--   The only downside is a restriction on MonadIO, but it is
--   unlikely that anyone will be using this in different context.
instance (MonadIO m, MonadMask m) => MonadMask (ReplT s m) where
  mask fma =
    withInterruptedContinuation $ ReplT $ \cont s-> mask $ \unmask-> unReplT (fma $ q unmask) cont s
    where
      q unmask ma = withInterruptedContinuation $ ReplT (\cont s-> unmask $ unReplT ma cont s)
  uninterruptibleMask fma =
    withInterruptedContinuation $ ReplT $ \cont s-> uninterruptibleMask $ \unmask-> unReplT (fma $ q unmask) cont s
    where
      q unmask ma = withInterruptedContinuation $ ReplT (\cont s-> unmask $ unReplT ma cont s)

-- | This causes the supplied computation to be called with
--   a terminating fake continuation.
--   The result is then evaluated and eventually the real continuation
--   is called. This is useful for isolating the effect of background
--   state like asynchronous exception masking state.
--   Note: The threaded state is representing all modifications
--   that happended within the supplied computation. This is essentially
--   the most important point of all this effort.
withInterruptedContinuation :: (MonadIO m) => ReplT s m a -> ReplT s m a
withInterruptedContinuation (ReplT action) = ReplT $ \cont s-> do
  ref <- liftIO $ newIORef Nothing
  s'  <- action (\r s'-> liftIO (writeIORef ref (Just r)) >> pure s') s
  liftIO (readIORef ref) >>= \case
    Nothing -> pure s'
    Just r  -> cont r s'
{-# INLINE withInterruptedContinuation #-}

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

instance (MonadCatch m, MonadTerminal m) => T.MonadTerminal (ReplT s m) where
  moveCursorUp                = lift . moveCursorUp
  moveCursorDown              = lift . moveCursorDown
  moveCursorForward           = lift . moveCursorForward
  moveCursorBackward          = lift . moveCursorBackward
  getCursorPosition           = lift   getCursorPosition
  setCursorPosition           = lift . setCursorPosition
  setVerticalCursorPosition   = lift . setVerticalCursorPosition
  setHorizontalCursorPosition = lift . setHorizontalCursorPosition
  saveCursorPosition          = lift   saveCursorPosition
  restoreCursorPosition       = lift   restoreCursorPosition
  showCursor                  = lift   showCursor
  hideCursor                  = lift   hideCursor
  clearLine                   = lift   clearLine
  getScreenSize               = lift   getScreenSize

instance (MonadCatch m, T.MonadInput m) => T.MonadInput (ReplT s m) where
  waitMapInterruptAndEvents = lift . waitMapInterruptAndEvents

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
      T.KeyEvent (T.CharKey 'D') mods
        | mods == T.ctrlKey -> do
            putLn
            flush
            quit
      -- On Enter this function returns the entered string to the caller.
      T.KeyEvent T.EnterKey mods
        | mods == mempty -> do
            putLn
            flush
            pure $ reverse xss ++ yss
      T.KeyEvent T.BackspaceKey mods
        | mods == mempty -> case xss of
            []     -> withStacks xss yss
            (x:xs) -> do
              moveCursorBackward 1
              putString yss
              putChar ' '
              moveCursorBackward (length yss + 1)
              flush
              withStacks xs yss
      KeyEvent (ArrowKey Leftwards) mods
        | mods == mempty -> case xss of
            []     -> withStacks xss yss
            (x:xs) -> do
              moveCursorBackward 1
              flush
              withStacks xs (x:yss)
      KeyEvent (ArrowKey Rightwards) mods
        | mods == mempty -> case yss of
            []     -> withStacks xss yss
            (y:ys) -> do
              moveCursorForward 1
              flush
              withStacks (y:xss) ys
      T.KeyEvent (T.CharKey c) mods
        | mods == mempty && (isPrint c || isSpace c) -> do
            putChar c
            flush
            withStacks (c:xss) yss
        | otherwise ->
            withStacks xss yss
      ev -> do
        --putStringLn (show ev)
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
      KeyEvent (CharKey 'D') mods | mods == ctrlKey -> do
        putLn
        flush
        quit
      -- On Enter this function returns the entered string to the caller.
      KeyEvent EnterKey _ -> do
        putLn
        flush
        pure $ reverse xs
      -- On Erase one character is removed from the right.
      KeyEvent BackspaceKey _ ->
        withStack $! drop 1 xs
      KeyEvent (CharKey x) mods | mods == mempty ->
        withStack $! x:xs
      _ -> withStack xs
