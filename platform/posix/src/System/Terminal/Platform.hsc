{-# LANGUAGE LambdaCase #-}
module System.Terminal.Platform
  ( withTerminal
  ) where

import           Control.Concurrent
import qualified Control.Concurrent.Async      as A
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TMVar
import qualified Control.Exception             as E
import           Control.Monad                 (forever, when, unless, void)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bits
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import           Data.Maybe
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           System.Environment
import qualified System.IO                     as IO
import qualified System.IO.Error               as IO
import qualified GHC.Conc                      as Conc
import qualified Data.Dynamic                  as Dyn
import           System.Posix.Types            (Fd(..))

import qualified Control.Monad.Terminal        as T
import qualified Control.Monad.Terminal.Ansi   as T

#include "Rts.h"
#include "hs_terminal.h"

withTerminal :: (MonadIO m, MonadMask m) => (T.Terminal -> m a) -> m a
withTerminal action = do
  termType    <- BS8.pack . fromMaybe "xterm" <$> liftIO (lookupEnv "TERM")
  mainThread  <- liftIO myThreadId
  interrupt   <- liftIO (newTVarIO False)
  output      <- liftIO newEmptyTMVarIO
  outputFlush <- liftIO newEmptyTMVarIO
  events      <- liftIO newTChanIO
  screenSize  <- liftIO (newTVarIO =<< getWindowSize)
  withTermiosSettings $
    withResizeHandler (atomically . writeTChan events . T.WindowEvent . T.WindowSizeChanged =<< getWindowSize) $
    withInputProcessing mainThread interrupt events $ 
    withOutputProcessing output outputFlush $ action $ T.Terminal {
        T.termType           = termType
      , T.termInputEvents    = readTChan events
      , T.termInterrupt      = swapTVar interrupt False >>= check
      , T.termOutput         = putTMVar output
      , T.termOutputFlush    = putTMVar outputFlush ()
      , T.termScreenSize     = readTVar screenSize
      }

withTermiosSettings :: (MonadIO m, MonadMask m) => m a -> m a
withTermiosSettings ma = bracket before after between
  where
    before  = liftIO (getTermios >>= \t-> setTermios t { termiosISIG = False, termiosICANON = False, termiosECHO = False } >> pure t)
    after   = liftIO . setTermios
    between = const ma

withResizeHandler :: (MonadIO m, MonadMask m) => IO () -> m a -> m a
withResizeHandler handler = bracket installHandler restoreHandler . const
  where
    installHandler = liftIO $ do
      Conc.ensureIOManagerIsRunning
      oldHandler <- Conc.setHandler (#const SIGWINCH) (Just (const handler, Dyn.toDyn handler))
      oldAction  <- stg_sig_install (#const SIGWINCH) (#const STG_SIG_HAN) nullPtr
      pure (oldHandler,oldAction)
    restoreHandler (oldHandler,oldAction) = liftIO $ do
      void $ Conc.setHandler (#const SIGWINCH) oldHandler
      void $ stg_sig_install (#const SIGWINCH) oldAction nullPtr
      pure ()

withOutputProcessing :: (MonadIO m, MonadMask m) => TMVar Text.Text -> TMVar () -> m a -> m a
withOutputProcessing output outputFlush = bracket
  ( liftIO $ A.async run )
  ( liftIO . A.cancel ) . const
  where
    run = forever $ atomically ((Just <$> takeTMVar output) `orElse` (takeTMVar outputFlush >> pure Nothing)) >>= \case
      Nothing -> IO.hFlush IO.stdout
      Just t  -> Text.hPutStr IO.stdout t

withInputProcessing :: (MonadIO m, MonadMask m) => ThreadId -> TVar Bool -> TChan T.Event -> m a -> m a
withInputProcessing mainThread interrupt events = bracket
  ( liftIO $ A.async run )
  ( liftIO . A.cancel ) . const
  where
    run :: IO ()
    run = do 
      termios <- getTermios
      forever $ do 
        IO.hGetChar handle >>= \case
          c | c == termiosVINTR  termios -> handleInterrupt c
            | c == termiosVERASE termios -> atomically $ writeChar c >> writeKey T.EraseKey
            | otherwise                  -> atomically $ writeChar c
        writeFillCharacterAfterTimeout

    handle     :: IO.Handle
    handle      = IO.stdin
    writeEvent :: T.Event -> STM ()
    writeEvent  = writeTChan events
    writeKey   :: T.Key -> STM ()
    writeKey k  = writeTChan events (T.KeyEvent k mempty)
    writeChar  :: Char -> STM ()
    writeChar c = writeTChan events (T.KeyEvent (T.CharKey c) mempty)
    -- This function is responsible for passing interrupt events and
    -- eventually throwing an exception to the main thread in case it
    -- detects that the main thread is not serving its duty to process
    -- interrupt events. It does this by setting a flag each time an interrupt
    -- occurs - if the flag is still set when a new interrupt occurs, it assumes
    -- the main thread is not responsive.
    handleInterrupt  :: Char -> IO ()
    handleInterrupt c =  do
      unhandledInterrupt <- liftIO (atomically $ writeChar c >> writeEvent T.InterruptEvent >> swapTVar interrupt True)
      when unhandledInterrupt (E.throwTo mainThread E.UserInterrupt)
    -- This function first evaluates whether more input is immediately available.
    -- If this is the case it just returns. Otherwise it registers interest in
    -- the file descriptor and waits for either input becoming available or a timeout
    -- to occur. When the timeout triggers, a NUL character is appended to the
    -- event stream to enable subsequent decoders to unambigously decode all
    -- cases without the need to take timing into consideration anymore.
    writeFillCharacterAfterTimeout :: IO ()
    writeFillCharacterAfterTimeout = do
      ready <- IO.hReady handle
      unless ready $ bracket (threadWaitReadSTM (Fd 0)) snd $ \(inputAvailable,_)-> do
        timeout <- registerDelay timeoutMicroseconds >>= \t-> pure (readTVar t >>= check)
        atomically $ inputAvailable `orElse` (timeout >> writeChar '\NUL')
    -- The timeout duration has been choosen as a tradeoff between correctness
    -- (actual transmission or scheduling delays shall not be misinterpreted) and
    -- responsiveness for a human user (50 ms are barely noticable, but 1000 ms are).
    -- I.e. when the user presses the ESC key (as vim users sometimes do ;-)
    -- it shall be reflected in the application behavior quite instantly and
    -- certainly _before_ the user presses the next key (thereby assuming that the
    -- user is not able to type more than 20 characters per second).
    -- For escape sequences it shall also be taken into consideration that they are
    -- usually transmitted and received as chunks. Only on very rare occasions (buffer
    -- boundaries) it might happen that they are split right after the sequence
    -- introducer. In a modern environment with virtual terminals there is good
    -- reason to consider this more unlikely than a user that types so fast
    -- that his input might be misinterpreted as an escape sequence.
    timeoutMicroseconds :: Int
    timeoutMicroseconds  = 50000

getWindowSize :: IO (Int, Int)
getWindowSize =
  alloca $ \ptr->
    unsafeIOCtl 0 (#const TIOCGWINSZ) ptr >>= \case
      0 -> peek ptr >>= \ws-> pure (fromIntegral $ wsRow ws, fromIntegral $ wsCol ws)
      _ -> undefined

getTermios :: IO Termios
getTermios = 
  alloca $ \ptr->
    unsafeGetTermios 0 ptr >>= \case
      0 -> peek ptr
      _ -> undefined

setTermios :: Termios -> IO ()
setTermios t =
  alloca $ \ptr->
    unsafeGetTermios 0 ptr >>= \case
      0 -> do 
        poke ptr t
        unsafeSetTermios 0 (#const TCSANOW) ptr >>= \case
          0 -> pure ()
          _ -> undefined
      _ -> undefined

data Winsize
  = Winsize
  { wsRow :: !CUShort
  , wsCol :: !CUShort
  } deriving (Eq, Ord, Show)

data Termios
  = Termios
  { termiosVEOF   :: !Char
  , termiosVERASE :: !Char
  , termiosVINTR  :: !Char
  , termiosVKILL  :: !Char
  , termiosVQUIT  :: !Char
  , termiosISIG   :: !Bool
  , termiosICANON :: !Bool
  , termiosECHO   :: !Bool
  } deriving (Eq, Ord, Show)

instance Storable Winsize where
  sizeOf    _ = (#size struct winsize)
  alignment _ = (#alignment struct winsize)
  peek ptr    = Winsize
    <$> (#peek struct winsize, ws_row) ptr
    <*> (#peek struct winsize, ws_col) ptr
  poke ptr ws = do
    (#poke struct winsize, ws_row) ptr (wsRow ws)
    (#poke struct winsize, ws_col) ptr (wsCol ws)

instance Storable Termios where
  sizeOf    _ = (#size struct termios)
  alignment _ = (#alignment struct termios)
  peek ptr    = do
    lflag <- peekLFlag
    Termios
      <$> (toEnum . fromIntegral <$> peekVEOF)
      <*> (toEnum . fromIntegral <$> peekVERASE)
      <*> (toEnum . fromIntegral <$> peekVINTR)
      <*> (toEnum . fromIntegral <$> peekVKILL)
      <*> (toEnum . fromIntegral <$> peekVQUIT)
      <*> pure (lflag .&. (#const ISIG)   /= 0)
      <*> pure (lflag .&. (#const ICANON) /= 0)
      <*> pure (lflag .&. (#const ECHO)   /= 0)
    where
      peekVEOF       = (#peek struct termios, c_cc[VEOF])   ptr :: IO CUChar
      peekVERASE     = (#peek struct termios, c_cc[VERASE]) ptr :: IO CUChar
      peekVINTR      = (#peek struct termios, c_cc[VINTR])  ptr :: IO CUChar
      peekVKILL      = (#peek struct termios, c_cc[VKILL])  ptr :: IO CUChar
      peekVQUIT      = (#peek struct termios, c_cc[VQUIT])  ptr :: IO CUChar
      peekLFlag      = (#peek struct termios, c_lflag)      ptr :: IO CUInt 
  poke ptr termios = do
    pokeVEOF   $ fromIntegral $ fromEnum $ termiosVEOF   termios
    pokeVERASE $ fromIntegral $ fromEnum $ termiosVERASE termios
    pokeVINTR  $ fromIntegral $ fromEnum $ termiosVINTR  termios
    pokeVKILL  $ fromIntegral $ fromEnum $ termiosVKILL  termios
    pokeVQUIT  $ fromIntegral $ fromEnum $ termiosVQUIT  termios
    peekLFlag >>= \flag-> pokeLFlag (if termiosISIG   termios then flag .|. (#const ISIG)   else flag .&. complement (#const ISIG))
    peekLFlag >>= \flag-> pokeLFlag (if termiosICANON termios then flag .|. (#const ICANON) else flag .&. complement (#const ICANON))
    peekLFlag >>= \flag-> pokeLFlag (if termiosECHO   termios then flag .|. (#const ECHO)   else flag .&. complement (#const ECHO))
    where
      pokeVEOF       = (#poke struct termios, c_cc[VEOF])   ptr :: CUChar -> IO ()
      pokeVERASE     = (#poke struct termios, c_cc[VERASE]) ptr :: CUChar -> IO ()
      pokeVINTR      = (#poke struct termios, c_cc[VINTR])  ptr :: CUChar -> IO ()
      pokeVKILL      = (#poke struct termios, c_cc[VKILL])  ptr :: CUChar -> IO ()
      pokeVQUIT      = (#poke struct termios, c_cc[VQUIT])  ptr :: CUChar -> IO ()
      pokeLFlag      = (#poke struct termios, c_lflag)      ptr :: CUInt -> IO ()
      peekLFlag      = (#peek struct termios, c_lflag)      ptr :: IO CUInt

foreign import ccall unsafe "tcgetattr"
  unsafeGetTermios :: CInt -> Ptr Termios -> IO CInt

foreign import ccall unsafe "tcsetattr"
  unsafeSetTermios :: CInt -> CInt -> Ptr Termios -> IO CInt

foreign import ccall unsafe "ioctl"
  unsafeIOCtl :: CInt -> CInt -> Ptr a -> IO CInt

foreign import ccall unsafe
  stg_sig_install :: CInt -> CInt -> Ptr a -> IO CInt
