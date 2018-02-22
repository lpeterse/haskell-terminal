{-# LANGUAGE LambdaCase #-}
module System.Terminal.Ansi.Platform
  ( withTerminal
  ) where

import Data.Maybe
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

#include "hs_terminal.h"

instance MonadInput (StateT BS.ByteString IO) where
  getNext = do
    st <- get
    case BS.uncons st of
      Just (b,bs) -> put bs >> pure b
      Nothing -> do
        ccs <- liftIO $ BS.hGetSome IO.stdin 1024
        put (BS.tail ccs)
        pure (BS.head ccs)
  getNextNonBlock = do
    st <- get
    case BS.uncons st of
      Just (b,bs) -> put bs >> pure (Just b)
      Nothing -> do
        ccs <- liftIO $ BS.hGetNonBlocking IO.stdin 1024
        case BS.uncons ccs of
          Just (c,cs) -> put cs >> pure (Just c)
          Nothing     -> pure Nothing
  wait = liftIO $ threadDelay 100000

withTerminal :: (MonadIO m, MonadMask m) => IO.Handle -> IO.Handle -> (T.TermEnv -> m a) -> m a
withTerminal hIn hOut action = do
  liftIO $ getTermios >>= print
  mainThreadId <- liftIO myThreadId
  eventChan <- liftIO newTChanIO
  screenSize <- liftIO (newTVarIO =<< hGetWindowSize hIn)
  hWithRawMode hIn $
   hWithoutEcho hIn $
    hWithHookedResizeSignal hIn eventChan $
      hWithInputProcessing hIn eventChan $
        withHookedInterruptSignal mainThreadId eventChan $
          \sigInt-> action $ T.TermEnv {
              T.envInput      = readTChan eventChan
            , T.envInterrupt  = sigInt
            , T.envScreenSize = readTVar screenSize
            , T.envCursorPosition = retry
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
    run = do 
      termios <- getTermios
      flip evalStateT BS.empty $ forever $
            liftIO . atomically . writeTChan eventChan =<< mapEvent termios <$> T.decodeAnsi

    mapEvent termios ev@(T.EvKey (T.KChar c) [])
      | c == '\NUL'                = T.EvKey T.KNull []
      | c == termiosVINTR  termios = T.EvKey (T.KChar 'C')  [T.MCtrl]
      | c == termiosVEOF   termios = T.EvKey (T.KChar 'D')  [T.MCtrl]
      | c == termiosVKILL  termios = T.EvKey (T.KChar 'U')  [T.MCtrl]
      | c == termiosVQUIT  termios = T.EvKey (T.KChar '\\') [T.MCtrl]
      | c == termiosVERASE termios = T.EvKey T.KErase []
      | c == '\DEL'                = T.EvKey T.KDelete []
      | c == '\b'                  = T.EvKey T.KDelete []
      | c == '\n'                  = T.EvKey T.KEnter []
      | c  < ' '                   = T.EvKey (T.KChar $ toEnum $ 64 + fromEnum c) [T.MCtrl]
      | otherwise                  = ev
    mapEvent termios ev            = ev

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

foreign import ccall unsafe "tcgetattr"
  unsafeGetTermios :: CInt -> Ptr Termios -> IO CInt

getTermios :: IO Termios
getTermios = 
  alloca $ \ptr->
    unsafeGetTermios 0 ptr >>= \case
      0 -> peek ptr
      _ -> pure defaultTermios

data Termios
  = Termios
  { termiosVEOF   :: !Char
  , termiosVERASE :: !Char
  , termiosVINTR  :: !Char
  , termiosVKILL  :: !Char
  , termiosVQUIT  :: !Char
  } deriving (Eq, Ord, Show)

defaultTermios :: Termios
defaultTermios = Termios
  { termiosVEOF   = '\EOT'
  , termiosVERASE = '\DEL'
  , termiosVINTR  = '\ETX'
  , termiosVKILL  = '\NAK'
  , termiosVQUIT  = '\FS'
  }

instance Storable Termios where
  sizeOf    _ = (#size struct termios)
  alignment _ = (#alignment struct termios)
  peek ptr    = Termios
    <$> (toEnum . fromIntegral <$> peekVEOF)
    <*> (toEnum . fromIntegral <$> peekVERASE)
    <*> (toEnum . fromIntegral <$> peekVINTR)
    <*> (toEnum . fromIntegral <$> peekVKILL)
    <*> (toEnum . fromIntegral <$> peekVQUIT)
    where
      peekVEOF       = (#peek struct termios, c_cc[VEOF])   ptr :: IO CUChar
      peekVERASE     = (#peek struct termios, c_cc[VERASE]) ptr :: IO CUChar
      peekVINTR      = (#peek struct termios, c_cc[VINTR])  ptr :: IO CUChar
      peekVKILL      = (#peek struct termios, c_cc[VKILL])  ptr :: IO CUChar
      peekVQUIT      = (#peek struct termios, c_cc[VQUIT])  ptr :: IO CUChar
  poke = undefined
