{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module System.Terminal.Ansi.AnsiTerminalT
  ( AnsiTerminalT ()
  , AnsiReplT (..)
  , runAnsiTerminalT
  , execAnsiReplT
  , evalAnsiReplT
  )
where

import           Control.Concurrent
import qualified Control.Concurrent.Async      as A
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import qualified Control.Exception             as E
import           Control.Monad                 (forever, void, when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bits
import qualified Data.ByteString               as BS
import           Data.Char
import           Data.Function                 (fix)
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import qualified Data.List.NonEmpty            as N
import           Data.Maybe
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Word
import           System.Environment
import qualified System.IO                     as IO

import qualified Control.Monad.Repl            as T
import qualified Control.Monad.Terminal        as T
import qualified Control.Monad.Terminal.Color  as T
import qualified Control.Monad.Terminal.Events as T
import qualified Control.Monad.Terminal.Modes  as T
import qualified Control.Monad.Terminal.Pretty as T

import qualified System.Terminal.Ansi.Internal as T
import qualified System.Terminal.Ansi.Platform as T

newtype AnsiTerminalT m a
  = AnsiTerminalT (ReaderT IsolationLevel (ReaderT (T.TermEnv, (Int, Output) -> STM ()) m) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

type AnsiReplT s m = T.ReplT s (AnsiTerminalT m)

execAnsiReplT :: AnsiReplT s IO () -> s -> IO s
execAnsiReplT ma = runAnsiTerminalT . T.execReplT ma

evalAnsiReplT :: AnsiReplT s IO () -> s -> IO ()
evalAnsiReplT ma = void . execAnsiReplT ma

runAnsiTerminalT :: AnsiTerminalT IO a -> IO a
runAnsiTerminalT = hRunAnsiTerminalT IO.stdin IO.stdout

hRunAnsiTerminalT :: IO.Handle -> IO.Handle -> AnsiTerminalT IO a -> IO a
hRunAnsiTerminalT hIn hOut (AnsiTerminalT ma) = T.withTerminal hIn hOut $ \env-> do
  outputChan    <- newEmptyTMVarIO :: IO (TMVar (Int,Output))
  A.withAsync (runOutput $ takeTMVar outputChan) $ \outputThread->
    runAction (env, putTMVar outputChan)
  where
    --runAction :: (STM T.Event, STM (), (Int, Output) -> STM ()) -> IO a
    runAction = runReaderT (runReaderT ma 0)

    runOutput :: STM (Int, Output) -> IO ()
    runOutput getOutput = run (defaultTerminalStateAttributes, []) `E.finally` print "OUTPUT DIED"
      where
        run :: (TerminalStateAttributes, [(Int, TerminalStateAttributes)]) -> IO ()
        run st = atomically getOutput >>= \(i,o)-> case o of
          OutIsolate            -> isolate1 i
          OutPutLn              -> isolate2 i putLn
          OutPutChar          c -> isolate2 i (putChar c)
          OutPutString        s -> isolate2 i (putString s)
          OutPutStringLn      s -> isolate2 i (putStringLn s)
          OutPutText          t -> isolate2 i (putText t)
          OutPutTextLn        t -> isolate2 i (putTextLn t)
          OutFlush            m -> isolate2 i (flush m)
          OutSetDefault         -> isolate3 i setDefault (const defaultTerminalStateAttributes)
          OutSetUnderline     b -> isolate3 i (setUnderline b) (\a-> a { tsUnderline = b })
          OutSetBold          b -> isolate3 i (setBold b) (\a-> a { tsBold = b })
          OutSetNegative      b -> isolate3 i (setNegative b) (\a-> a { tsNegative = b })
          OutSetForeground    c -> isolate3 i (setForeground c)  (\a-> a { tsForeground = c })
          OutSetBackground    c -> isolate3 i (setBackground c) (\a-> a { tsBackground = c })
          OutClear              -> isolate2 i clear
          OutCursorUp         i -> isolate2 i (cursorUp i)
          OutCursorDown       i -> isolate2 i (cursorDown i)
          OutCursorForward    i -> isolate2 i (cursorForward i)
          OutCursorBackward   i -> isolate2 i (cursorBackward i)
          OutCursorPosition x y -> isolate2 i (cursorPosition x y)
          OutCursorVisible    b -> isolate3 i (cursorVisible b) (\a-> a { tsCursorVisible = b})
          OutAskCursorPosition  -> isolate2 i askCursorPosition
          OutSetLineWrap      b -> isolate2 i (setLineWrap b)
          where
            isolate1 i     = isolate True  i st >>= run
            isolate2 i m   = isolate False i st >>= \st-> m >> run st
            isolate3 i m f = isolate False i st >>= \(cur, st)-> m >> run (f cur, st)

        putDiff :: TerminalStateAttributes -> TerminalStateAttributes -> IO ()
        putDiff a b = x1 >> x2 >> x3 >> x4 >> x5 >> x6
          where
            x1 = when (tsNegative      a /= tsNegative      b) $ setNegative   $ tsNegative      a
            x2 = when (tsUnderline     a /= tsUnderline     b) $ setUnderline  $ tsUnderline     a
            x3 = when (tsForeground    a /= tsForeground    b) $ setForeground $ tsForeground    a
            x4 = when (tsBackground    a /= tsBackground    b) $ setBackground $ tsBackground    a
            x5 = when (tsCursorVisible a /= tsCursorVisible b) $ cursorVisible $ tsCursorVisible a
            x6 = when (tsBold          a /= tsBold          b) $ setBold       $ tsBold          a

        isolate :: Bool -> Int -> (TerminalStateAttributes, [(Int, TerminalStateAttributes)]) -> IO (TerminalStateAttributes, [(Int, TerminalStateAttributes)])
        isolate hard 0 st@(current, [])
          | hard      = do -- As there is no older context on the stack, restore to default state.
                        putDiff defaultTerminalStateAttributes current
                        pure (defaultTerminalStateAttributes, [])
          | otherwise = do -- Being in default state here. Do nothing.
                        pure st
        isolate hard i st@(current, [])
          | hard      = do -- Was in default state, now getting into state > 0. Push current state on stack.
                        pure (current, [(i,current)])
          | otherwise = do -- Should be impossible. Do nothing.
                        pure st
        isolate hard i st@(current, (j,preserved):stack)
          | i > j     = do -- Diving down: A new nested context. Push the current state onto the stack.
                        pure (current, (i,current):stack)
          | i < j     = do -- Diving up: Recover an older context deeper in the stack.
                        isolate True i (current, stack)
          | hard      = do -- Same index, other context: Restore the context on top of the stack and make it the new current one.
                        putDiff preserved current
                        pure (preserved, (j,preserved):stack)
          | otherwise = do -- Same index, same context: Do nothing - this is caused by subsequent operations in the same context.
                        pure st

        putLn                                           = IO.putStr "\n"
        putChar c                                       = when (isPrint c || isSpace c) $ IO.putChar c
        putString                                       = IO.putStr . filter (\c-> isPrint c || isSpace c)
        putStringLn s                                   = putString s >> putLn
        putText                                         = Text.putStr . Text.filter (\c-> isPrint c || isSpace c)
        putTextLn t                                     = putText t >> putLn
        flush confirm                                   = IO.hFlush IO.stdout >> atomically confirm

        setUnderline                               True = IO.putStr "\ESC[4m"
        setUnderline                              False = IO.putStr "\ESC[24m"
        setBold                                    True = IO.putStr "\ESC[1m"
        setBold                                   False = IO.putStr "\ESC[22m"
        setNegative                                True = IO.putStr "\ESC[7m"
        setNegative                               False = IO.putStr "\ESC[27m"
        setDefault                                      = IO.putStr "\ESC[m"
        setForeground c@T.ColorDefault                  = IO.putStr "\ESC[39m"
        setForeground c@(T.Color4Bit T.Black     False) = IO.putStr "\ESC[30m"
        setForeground c@(T.Color4Bit T.Red       False) = IO.putStr "\ESC[31m"
        setForeground c@(T.Color4Bit T.Green     False) = IO.putStr "\ESC[32m"
        setForeground c@(T.Color4Bit T.Yellow    False) = IO.putStr "\ESC[33m"
        setForeground c@(T.Color4Bit T.Blue      False) = IO.putStr "\ESC[34m"
        setForeground c@(T.Color4Bit T.Magenta   False) = IO.putStr "\ESC[35m"
        setForeground c@(T.Color4Bit T.Cyan      False) = IO.putStr "\ESC[36m"
        setForeground c@(T.Color4Bit T.White     False) = IO.putStr "\ESC[37m"
        setForeground c@(T.Color4Bit T.Black      True) = IO.putStr "\ESC[90m"
        setForeground c@(T.Color4Bit T.Red        True) = IO.putStr "\ESC[91m"
        setForeground c@(T.Color4Bit T.Green      True) = IO.putStr "\ESC[92m"
        setForeground c@(T.Color4Bit T.Yellow     True) = IO.putStr "\ESC[93m"
        setForeground c@(T.Color4Bit T.Blue       True) = IO.putStr "\ESC[94m"
        setForeground c@(T.Color4Bit T.Magenta    True) = IO.putStr "\ESC[95m"
        setForeground c@(T.Color4Bit T.Cyan       True) = IO.putStr "\ESC[96m"
        setForeground c@(T.Color4Bit T.White      True) = IO.putStr "\ESC[97m"
        setForeground _                                 = error "FIXME"
        setBackground c@T.ColorDefault                  = IO.putStr "\ESC[49m"
        setBackground c@(T.Color4Bit T.Black     False) = IO.putStr "\ESC[40m"
        setBackground c@(T.Color4Bit T.Red       False) = IO.putStr "\ESC[41m"
        setBackground c@(T.Color4Bit T.Green     False) = IO.putStr "\ESC[42m"
        setBackground c@(T.Color4Bit T.Yellow    False) = IO.putStr "\ESC[43m"
        setBackground c@(T.Color4Bit T.Blue      False) = IO.putStr "\ESC[44m"
        setBackground c@(T.Color4Bit T.Magenta   False) = IO.putStr "\ESC[45m"
        setBackground c@(T.Color4Bit T.Cyan      False) = IO.putStr "\ESC[46m"
        setBackground c@(T.Color4Bit T.White     False) = IO.putStr "\ESC[47m"
        setBackground c@(T.Color4Bit T.Black      True) = IO.putStr "\ESC[100m"
        setBackground c@(T.Color4Bit T.Red        True) = IO.putStr "\ESC[101m"
        setBackground c@(T.Color4Bit T.Green      True) = IO.putStr "\ESC[102m"
        setBackground c@(T.Color4Bit T.Yellow     True) = IO.putStr "\ESC[103m"
        setBackground c@(T.Color4Bit T.Blue       True) = IO.putStr "\ESC[104m"
        setBackground c@(T.Color4Bit T.Magenta    True) = IO.putStr "\ESC[105m"
        setBackground c@(T.Color4Bit T.Cyan       True) = IO.putStr "\ESC[106m"
        setBackground c@(T.Color4Bit T.White      True) = IO.putStr "\ESC[107m"
        setBackground _                                 = error "FIXME"

        clear                                           = IO.putStr "\ESC[H"
        cursorUp i                                      = IO.putStr $ "\ESC[" ++ show (safeN i) ++ "A"
        cursorDown i                                    = IO.putStr $ "\ESC[" ++ show (safeN i) ++ "B"
        cursorForward i                                 = IO.putStr $ "\ESC[" ++ show (safeN i) ++ "C"
        cursorBackward i                                = IO.putStr $ "\ESC[" ++ show (safeN i) ++ "D"
        cursorPosition x y                              = IO.putStr $ "\ESC[" ++ show (safeN x) ++  ";" ++ show (safeN y) ++ "H"
        cursorVisible                             False = IO.putStr "\ESC[?25l"
        cursorVisible                              True = IO.putStr "\ESC[?25h"
        askCursorPosition                               = IO.putStr "\ESC[6n"
        setLineWrap                               False = IO.putStr "\ESC[7l"
        setLineWrap                                True = IO.putStr "\ESC[7h"

        safeN :: Int -> Int
        safeN n
          | n >= 1 && n <= 32767 = n
          | otherwise            = 1

type IsolationLevel = Int

data TerminalStateAttributes
  = TerminalStateAttributes
  { tsNegative      :: !Bool
  , tsUnderline     :: !Bool
  , tsBold          :: !Bool
  , tsCursorVisible :: !Bool
  , tsForeground    :: !T.Color
  , tsBackground    :: !T.Color
  } deriving (Eq, Ord, Show)

defaultTerminalStateAttributes :: TerminalStateAttributes
defaultTerminalStateAttributes =
  TerminalStateAttributes
  { tsNegative        = False
  , tsUnderline       = False
  , tsBold            = False
  , tsCursorVisible   = True
  , tsForeground      = T.ColorDefault
  , tsBackground      = T.ColorDefault
  }

instance MonadTrans AnsiTerminalT where
  lift = AnsiTerminalT . lift . lift

instance (MonadIO m, MonadThrow m) => T.MonadTerminal (AnsiTerminalT m) where

instance MonadIO m => T.MonadEvent (AnsiTerminalT m) where
  withEventSTM f = AnsiTerminalT $ do
    env <- fst <$> lift ask
    liftIO $ fix $ \again-> do
      let resetInterrupt = T.envInterrupt env >> pure Nothing
      let getUserEvent = Just <$> f (T.envInput env)
      atomically (resetInterrupt `orElse` getUserEvent) >>= \case
        Nothing -> again
        Just ev -> pure ev

instance (MonadIO m, MonadThrow m) => T.MonadIsolate (AnsiTerminalT m) where
  isolate (AnsiTerminalT ma) =
    AnsiTerminalT $ local (+ 1) (mu >> ma)
    where
      AnsiTerminalT mu = write OutIsolate

instance (MonadIO m, MonadThrow m) => T.MonadPrinter (AnsiTerminalT m) where
  putLn                 = write   OutPutLn
  putChar               = write . OutPutChar
  putString = \case
    [] -> pure ()
    (x:xs) -> T.putChar x >> T.putString xs
  putStringLn s         = T.putString s >> T.putLn
  putText               = write . OutPutText
  putTextLn             = write . OutPutTextLn
  flush                 = do
    tv <- liftIO (newTVarIO False)
    write $ OutFlush (writeTVar tv True)
    liftIO (atomically $ readTVar tv >>= check)

instance (MonadIO m, MonadThrow m) => T.MonadColorPrinter (AnsiTerminalT m) where
  setDefault            = write   OutSetDefault
  setForegroundColor    = write . OutSetForeground
  setBackgroundColor    = write . OutSetBackground
  setUnderline          = write . OutSetUnderline
  setBold               = write . OutSetBold
  setNegative           = write . OutSetNegative

instance (MonadIO m, MonadThrow m) => T.MonadScreen (AnsiTerminalT m) where
  clear                 = write   OutClear
  cursorUp              = write . OutCursorUp
  cursorDown            = write . OutCursorDown
  cursorForward         = write . OutCursorForward
  cursorBackward        = write . OutCursorBackward
  cursorPosition x      = write . OutCursorPosition x
  cursorVisible         = write . OutCursorVisible
  getScreenSize         = AnsiTerminalT $ do
    env <- fst <$> lift ask
    liftIO (atomically $ T.envScreenSize env)
  getCursorPosition     = do
    env <- AnsiTerminalT $ fst <$> lift ask
    write OutAskCursorPosition
    T.flush
    liftIO $ atomically $ T.envCursorPosition env
  setLineWrap           = write . OutSetLineWrap

data Output
   = OutIsolate
   | OutAskCursorPosition
   | OutPutLn
   | OutPutChar        Char
   | OutPutString      String
   | OutPutStringLn    String
   | OutPutText        Text.Text
   | OutPutTextLn      Text.Text
   | OutFlush          (STM ())
   | OutSetDefault
   | OutSetUnderline   Bool
   | OutSetBold        Bool
   | OutSetNegative    Bool
   | OutSetForeground  T.Color
   | OutSetBackground  T.Color
   | OutClear
   | OutCursorUp       Int
   | OutCursorDown     Int
   | OutCursorForward  Int
   | OutCursorBackward Int
   | OutCursorPosition Int Int
   | OutCursorVisible  Bool
   | OutSetLineWrap    Bool

write :: (MonadIO m, MonadThrow m) => Output -> AnsiTerminalT m ()
write output = AnsiTerminalT $ do
  isolationLevel <- ask
  (env,writeSTM) <- lift ask
  b <- liftIO $ atomically $
    (T.envInterrupt env >> pure True) `orElse` (writeSTM (isolationLevel, output) >> pure False)
  when b (throwM E.UserInterrupt)
