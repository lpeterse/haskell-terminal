{-# LANGUAGE LambdaCase #-}
module System.Terminal.Ansi.Platform
  ( TermEnv (..)
  , withTerminal
  ) where

import           Control.Concurrent
import qualified Control.Concurrent.Async      as A
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception             as E
import           Control.Monad                 (forever, when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import qualified Data.ByteString               as BS
import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified GHC.IO.FD                     as IO
import qualified GHC.IO.Handle.FD              as IO
import qualified System.IO                     as IO
import qualified System.IO.Error               as IO
import qualified System.Posix.Signals          as Posix
import qualified System.Posix.Signals.Exts     as Posix

import qualified Control.Monad.Terminal.Events as T
import qualified System.Terminal.Ansi.Internal as T

data TermEnv
  = TermEnv
  { envInput      :: STM T.Event
  , envInterrupt  :: STM ()
  , envScreenSize :: STM (Int,Int)
  }

withTerminal :: (MonadIO m, MonadMask m) => IO.Handle -> IO.Handle -> (TermEnv -> m a) -> m a
withTerminal hIn hOut action = do
  mainThreadId <- liftIO myThreadId
  eventChan <- liftIO newTChanIO
  screenSize <- liftIO (newTVarIO =<< hGetWindowSize hIn)
  hWithRawMode hIn $
   hWithoutEcho hIn $
    hWithHookedResizeSignal hIn eventChan $
      hWithInputProcessing hIn eventChan $
        withHookedInterruptSignal mainThreadId eventChan $
          \sigInt-> action $ TermEnv {
              envInput      = readTChan eventChan
            , envInterrupt  = sigInt
            , envScreenSize = readTVar screenSize
            }

withHookedInterruptSignal :: (MonadIO m, MonadMask m) => ThreadId -> (TChan T.Event) -> (STM () -> m a) -> m a
withHookedInterruptSignal mainThreadId eventChan action = do
  sig <- liftIO (newTVarIO False)
  bracket
    (liftIO $ flip (Posix.installHandler Posix.sigINT) Nothing  $ Posix.Catch $ passInterrupt sig)
    (liftIO . flip (Posix.installHandler Posix.sigINT) Nothing) $ const $ action $ resetInterrupt sig
  where
    passInterrupt sig = do
      unhandledInterrupt <- atomically (swapTVar sig True)
      when unhandledInterrupt $ do
        E.throwTo mainThreadId E.UserInterrupt
      atomically (writeTChan eventChan $ T.EvKey (T.KChar 'C') [T.MCtrl])
    resetInterrupt sig = do
      readTVar sig >>= check
      writeTVar sig False

hWithHookedResizeSignal :: (MonadIO m, MonadMask m) => IO.Handle -> (TChan T.Event) -> m a -> m a
hWithHookedResizeSignal h eventChan action =
  bracket
    (liftIO $ flip (Posix.installHandler Posix.windowChange) Nothing  $ Posix.Catch pushEvent)
    (liftIO . flip (Posix.installHandler Posix.windowChange) Nothing) $ const $ action
  where
    pushEvent = do
      ev <- T.EvResize <$> hGetWindowSize h
      atomically (writeTChan eventChan $ ev)

hWithoutEcho :: (MonadIO m, MonadMask m) => IO.Handle -> m a -> m a
hWithoutEcho h = bracket
  (liftIO $ IO.hGetEcho h >>= \x-> IO.hSetEcho h False >> pure x)
  (liftIO . IO.hSetEcho h) . const

hWithRawMode :: (MonadIO m, MonadMask m) => IO.Handle -> m a -> m a
hWithRawMode h = bracket
  (liftIO $ IO.hGetBuffering h >>= \b-> IO.hSetBuffering h IO.NoBuffering >> pure b)
  (liftIO . IO.hSetBuffering h) . const

-- | ...
--
--   FIXME: `Handle` parameter is ignored right now and `IO.stdin` used instead.
hWithInputProcessing ::  (MonadIO m, MonadMask m) => IO.Handle -> (TChan T.Event) -> m a -> m a
hWithInputProcessing h eventChan = bracket
  ( liftIO $ A.async run )
  ( liftIO . A.cancel ) . const
  where
    run = flip evalStateT BS.empty $ forever $
            liftIO . atomically . writeTChan eventChan =<< mapEvent <$> T.decodeAnsi

    mapEvent (T.EvKey (T.KChar '\DEL') [])     = T.EvKey (T.KBackspace 1) []
    mapEvent (T.EvKey (T.KChar 'J') [T.MCtrl]) = T.EvKey T.KEnter []
    mapEvent ev                                = ev

hGetWindowSize :: IO.Handle -> IO (Int, Int)
hGetWindowSize h = do
  fd <- IO.handleToFd h
  alloca $ \rowsPtr-> alloca $ \colsPtr->
    unsafeGetWindowSize (IO.fdFD fd) rowsPtr colsPtr >>= \case
      0 -> do
        rows <- peek rowsPtr
        cols <- peek colsPtr
        pure (fromIntegral rows, fromIntegral cols)
      _ -> pure (-1,-1)

foreign import ccall unsafe "hs_get_winsize"
  unsafeGetWindowSize :: CInt -> Ptr Word16 -> Ptr Word16 -> IO Int
