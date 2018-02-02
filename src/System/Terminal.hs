{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module System.Terminal
    ( --AnsiTermT (..)
    --, runAnsiTermT
      withRawMode
    , withoutEcho
    , runInputT
    , module E
    , module T
    , module M
    ) where

import           Control.Concurrent.Async     (waitSTM, withAsync)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Exception            (AsyncException (..),
                                               SomeException, bracket, catch,
                                               finally, throwIO)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.STM
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Bits
import qualified Data.ByteString              as BS
import           Data.Word
import           Prelude                      hiding (getChar, getLine, putChar,
                                               putStr)
import qualified Prelude                      as P
import           System.Environment
import           System.IO
--import           System.Posix.Signals
import qualified System.Terminal.Events       as E
import qualified System.Terminal.Input        as T
import qualified System.Terminal.Modes        as M

data AnsiTermState
  = AnsiTermState
  { isInterruptFlag :: TVar Bool
  }

newtype AnsiTermT m a = AnsiTermT (ReaderT AnsiTermState m a)
  deriving (Functor, Applicative, Monad, MonadIO)

runInputT :: Monad m => T.InputT m a -> m a
runInputT (T.InputT m) = evalStateT m BS.empty

{-
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

  action interruptFlag = runReaderT r AnsiTermState {
      isInterruptFlag = interruptFlag
    }
-}

withoutEcho :: IO a -> IO a
withoutEcho = bracket
  (hGetEcho stdin >>= \x-> hSetEcho stdin False >> pure x)
  (hSetEcho stdin) . const

withRawMode :: IO a -> IO a
withRawMode = bracket
  (hGetBuffering stdin >>= \b-> hSetBuffering stdin NoBuffering >> pure b)
  (hSetBuffering stdin) . const


