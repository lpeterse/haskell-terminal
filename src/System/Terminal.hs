{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module System.Terminal
    ( MonadTerm (..)
    , AnsiTermT (..)
    , runAnsiTermT
    ) where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Exception            (AsyncException (..),
                                               SomeException, bracket, catch,
                                               finally, throwIO)
import           Control.Monad                (forever)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.STM
import           Control.Monad.Trans
import           Data.Function                (fix)
import           Data.Word
import           GHC.Conc.Signal
import qualified System.Console.ANSI          as A
import           System.IO
import           System.Posix.Signals

import           Prelude                      hiding (getChar, getLine, putChar,
                                               putStr)
import qualified Prelude                      as P

data Color
  = Color { colorR :: !Word8, colorG :: !Word8, colorB :: !Word8 }
  deriving (Eq, Ord, Show)

data AnsiTermState
  = AnsiTermState
  { isInterruptFlag :: TVar Bool
  }

newtype AnsiTermT m a = AnsiTermT (ReaderT AnsiTermState m a)
  deriving (Functor, Applicative, Monad, MonadIO)

class MonadPrinter m where
  printChar            :: Char -> m ()
  printString          :: String -> m ()
  printStringRaw       :: String -> m ()
  cr                   :: m ()
  lf                   :: m ()
  nl                   :: m ()
  setForegroundColor   :: Color -> m ()
  setBackgroundColor   :: Color -> m ()
  setBold              :: m ()
  setUnderline         :: m ()
  reset                :: m ()

class MonadTerm m where
  askInterruptEvent    :: m (STM ())
  moveCursorVertical   :: Int -> m ()
  moveCursorHorizontal :: Int -> m ()
  clearScreen          :: m ()

instance MonadIO m => MonadPrinter (AnsiTermT m) where
  printChar
    = AnsiTermT . liftIO . P.putChar
  printString
    = AnsiTermT . liftIO . P.putStr

instance MonadIO m => MonadTerm (AnsiTermT m) where
  -- As soon as the interrupt flag becomes true, unblock and set it to false again.
  askInterruptEvent
    = AnsiTermT $ do
        flag <- isInterruptFlag <$> ask
        pure $ readTVar flag >>= \x-> check x >> writeTVar flag False
  moveCursorVertical i
    | i < 0     = AnsiTermT $ liftIO $ A.cursorBackward (negate i)
    | i > 0     = AnsiTermT $ liftIO $ A.cursorForward i
    | otherwise = pure ()
  moveCursorHorizontal i
    | i < 0     = AnsiTermT $ liftIO $ A.cursorUp (negate i)
    | i > 0     = AnsiTermT $ liftIO $ A.cursorDown i
    | otherwise = pure ()
  clearScreen
    = AnsiTermT $ liftIO $ A.clearScreen

runAnsiTermT :: AnsiTermT IO a -> IO a
runAnsiTermT (AnsiTermT r) = do
  interruptFlag <- newTVarIO False
  withoutEcho $
    withRawMode $
      withHookedSignals $ \waitSignal->
        withAsync (action interruptFlag) $ \actionT-> fix $ \proceed-> do
          let waitAction = Just <$> waitSTM actionT
          let waitInterrupt = waitSignal >> swapTVar interruptFlag True >>= \case
                True  -> throwSTM UserInterrupt -- No one has handled an earlier interrupt - exit!
                False -> pure Nothing
          atomically (waitAction `orElse` waitInterrupt) >>= \case
                Just a  -> pure a
                Nothing -> proceed
  where
  withHookedSignals :: (STM () -> IO a) -> IO a
  withHookedSignals action = do
    sig <- newTVarIO False
    bracket
      (flip (installHandler sigINT) Nothing  $ Catch $ atomically $ writeTVar sig True)
      (flip (installHandler sigINT) Nothing) $ const $ action (readTVar sig >>= check >> writeTVar sig False)

  withoutEcho :: IO a -> IO a
  withoutEcho = bracket
    (hGetEcho stdin >>= \x-> hSetEcho stdin False >> pure x)
    (hSetEcho stdin) . const

  withRawMode :: IO a -> IO a
  withRawMode = bracket
    (hGetBuffering stdin >>= \b-> hSetBuffering stdin NoBuffering >> pure b)
    (hSetBuffering stdin) . const

  action interruptFlag = runReaderT r AnsiTermState {
        isInterruptFlag = interruptFlag
      }

