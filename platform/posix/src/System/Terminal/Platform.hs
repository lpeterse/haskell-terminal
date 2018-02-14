{-# LANGUAGE LambdaCase #-}
module System.Terminal.Platform
  ( Signals (..)
  , withTerminal
  , gatherInputEvents
  ) where

import           Control.Concurrent
import qualified Control.Concurrent.Async    as A
import           Control.Concurrent.STM.TVar
import           Control.Exception
import qualified Control.Exception           as E
import           Control.Monad               (forever, when)
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import qualified Data.ByteString             as BS
import           Data.Word
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified System.IO                   as IO
import qualified System.Posix.Signals        as Posix
import qualified System.Posix.Signals.Exts   as Posix

import qualified System.Terminal.Ansi        as T
import qualified System.Terminal.Events      as T

data Signals
  = Signals
  { sigInterrupt    :: STM ()
  , sigWindowChange :: STM (Int, Int)
  }

withTerminal :: (Signals -> IO a) -> IO a
withTerminal action =
  withRawMode $
   withoutEcho $
     withHookedInterruptSignal $ \sigIntr->
      withHookedWindowChangeSignal $ \sigWinch->
        action $ Signals sigIntr sigWinch

withHookedInterruptSignal :: (STM () -> IO a) -> IO a
withHookedInterruptSignal action = do
  sig <- newTVarIO False
  bracket
    (flip (Posix.installHandler Posix.sigINT) Nothing  $ Posix.Catch $ atomically $ writeTVar sig True)
    (flip (Posix.installHandler Posix.sigINT) Nothing) $ const $ action (readTVar sig >>= check >> writeTVar sig False)

withHookedWindowChangeSignal :: (STM (Int, Int) -> IO a) -> IO a
withHookedWindowChangeSignal action = do
  sig <- newTVarIO Nothing
  bracket
    (flip (Posix.installHandler Posix.windowChange) Nothing  $ Posix.Catch $ getWindowSize >>= \ws-> atomically (writeTVar sig $ Just ws))
    (flip (Posix.installHandler Posix.windowChange) Nothing) $ const $ action (f sig)
  where
    f sig = do
      swapTVar sig Nothing >>= \case
        Nothing -> retry
        Just (r,c) -> pure (fromIntegral r, fromIntegral c)

withoutEcho :: IO a -> IO a
withoutEcho = bracket
  (IO.hGetEcho IO.stdin >>= \x-> IO.hSetEcho IO.stdin False >> pure x)
  (IO.hSetEcho IO.stdin) . const

withRawMode :: IO a -> IO a
withRawMode = bracket
  (IO.hGetBuffering IO.stdin >>= \b-> IO.hSetBuffering IO.stdin IO.NoBuffering >> pure b)
  (IO.hSetBuffering IO.stdin) . const

gatherInputEvents :: (T.Event -> STM ()) -> IO ()
gatherInputEvents push = flip evalStateT BS.empty $ forever $
  liftIO . atomically . push =<< mapEvent <$> T.decodeAnsi
  where
    mapEvent (T.EvKey (T.KChar '\DEL') [])     = T.EvKey (T.KBackspace 1) []
    mapEvent (T.EvKey (T.KChar 'J') [T.MCtrl]) = T.EvKey T.KEnter []
    mapEvent ev                                = ev

getWindowSize :: IO (Word16, Word16)
getWindowSize = alloca $ \rowsPtr-> alloca $ \colsPtr-> do
  getWinSize rowsPtr colsPtr
  rows <- peek rowsPtr
  cols <- peek colsPtr
  pure (rows, cols)

foreign import ccall unsafe "hs_get_winsize"
  getWinSize :: Ptr Word16 -> Ptr Word16 -> IO ()
