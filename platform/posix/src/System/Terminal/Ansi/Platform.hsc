{-# LANGUAGE LambdaCase #-}
module System.Terminal.Ansi.Platform
  ( withTerminal
  ) where

import           Control.Concurrent
import qualified Control.Concurrent.Async      as A
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TMVar
import qualified Control.Exception             as E
import           Control.Monad                 (forever, when, void)
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

import qualified Control.Monad.Terminal        as T
import qualified Control.Monad.Terminal.Ansi   as T

#include "Rts.h"
#include "hs_terminal.h"

withTerminal :: (MonadIO m, MonadMask m) => (T.AnsiTerminal -> m a) -> m a
withTerminal action = do
  termType    <- BS8.pack . fromMaybe "xterm" <$> liftIO (lookupEnv "TERM")
  mainThread  <- liftIO myThreadId
  interrupt   <- liftIO (newTVarIO False)
  chars       <- liftIO newTChanIO
  output      <- liftIO newEmptyTMVarIO
  outputFlush <- liftIO newEmptyTMVarIO
  events      <- liftIO newTChanIO
  screenSize  <- liftIO (newTVarIO =<< getWindowSize)
  withTermiosSettings $
    withResizeHandler (atomically . writeTChan events . T.WindowEvent . T.WindowSizeChanged =<< getWindowSize) $
    withInputProcessing mainThread interrupt chars events $ 
    withOutputProcessing output outputFlush $ action $ T.AnsiTerminal {
        T.ansiTermType       = termType
      , T.ansiInputChars     = readTChan chars
      , T.ansiInputEvents    = readTChan events
      , T.ansiInterrupt      = swapTVar interrupt False >>= check
      , T.ansiOutput         = putTMVar output
      , T.ansiOutputFlush    = putTMVar outputFlush ()
      , T.ansiScreenSize     = readTVar screenSize
      , T.ansiSpecialChars   = const Nothing
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

withInputProcessing :: (MonadIO m, MonadMask m) => ThreadId -> TVar Bool -> TChan Char -> TChan T.Event -> m a -> m a
withInputProcessing mainThread interrupt chars events = bracket
  ( liftIO $ A.async run )
  ( liftIO . A.cancel ) . const
  where
    run = do 
      termios <- getTermios
      forever $ do
        c <- IO.getChar
        case specialChar termios c of
          Nothing -> liftIO (atomically $ writeTChan chars c)
          Just ev -> case ev of
            T.InterruptEvent -> do
              unhandledInterrupt <- liftIO (atomically $ writeTChan events T.InterruptEvent >> swapTVar interrupt True)
              when unhandledInterrupt (E.throwTo mainThread E.UserInterrupt)
            _                -> liftIO (atomically $ writeTChan events ev)
    specialChar termios c
      | c == '\NUL'                = Just $ T.EvKey T.KNull []
      | c == termiosVINTR  termios = Just $ T.InterruptEvent
      | c == termiosVEOF   termios = Just $ T.EvKey (T.KChar 'D')  [T.MCtrl]
      | c == termiosVKILL  termios = Just $ T.EvKey (T.KChar 'U')  [T.MCtrl]
      | c == termiosVQUIT  termios = Just $ T.EvKey (T.KChar '\\') [T.MCtrl]
      | c == termiosVERASE termios = Just $ T.EvKey T.KErase  []
      | c == '\DEL'                = Just $ T.EvKey T.KDelete []
      | c == '\b'                  = Just $ T.EvKey T.KDelete []
      | c == '\n'                  = Just $ T.EvKey T.KEnter  []
      | otherwise                  = Nothing

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
