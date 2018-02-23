{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module System.Terminal.Ansi.RemoteTerminalT
  ( RemoteTerminalT ()
  , runRemoteTerminalT
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
import           Data.Monoid
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

data RemoteEnv
  = RemoteEnv
  { envTerm         :: BS.ByteString
  , envInputChars   :: STM Char
  , envInputEvents  :: STM T.Event
  , envOutput       :: Text.Text -> STM ()
  , envOutputFlush  :: STM ()
  , envSpecialChars :: Char -> Maybe T.Event
  }

newtype RemoteTerminalT m a
  = RemoteTerminalT (ReaderT RemoteEnv m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runRemoteTerminalT :: (MonadIO m, MonadMask m) => RemoteTerminalT m a -> RemoteEnv -> m a
runRemoteTerminalT (RemoteTerminalT action) env =
  runReaderT action env { envInputEvents = events }
  where
    events = (mapEvent <$> runReaderT T.decodeAnsi (envInputChars env)) `orElse` envInputEvents env
    mapEvent ev@(T.EvKey (T.KChar c) [])
      | c == '\NUL' = T.EvKey T.KNull []
      | c  < ' '    = fromMaybe (T.EvKey (T.KChar $ toEnum $ 64 + fromEnum c) [T.MCtrl]) (envSpecialChars env c)
      | otherwise   = ev
    mapEvent ev = ev

instance MonadTrans RemoteTerminalT where
  lift = RemoteTerminalT . lift

instance (MonadIO m) => T.MonadEvent (RemoteTerminalT m) where
  withEventSTM f = RemoteTerminalT (liftIO . atomically . f . envInputEvents =<< ask)

instance (MonadIO m) => T.MonadPrinter (RemoteTerminalT m) where
  putChar c = RemoteTerminalT $ do
    env <- ask
    liftIO $ atomically $ envOutput env $ Text.singleton c
  putString = \case
    [] -> pure ()
    (x:xs) -> T.putChar x >> T.putString xs
  putText t = RemoteTerminalT $ do
    env <- ask
    liftIO $ atomically $ envOutput env t
  flush = RemoteTerminalT $ do
    env <- ask
    liftIO  $ atomically $ envOutputFlush env

instance (MonadIO m) => T.MonadColorPrinter (RemoteTerminalT m) where
  setUnderline                               True = write "\ESC[4m"
  setUnderline                              False = write "\ESC[24m"
  setBold                                    True = write "\ESC[1m"
  setBold                                   False = write "\ESC[22m"
  setNegative                                True = write "\ESC[7m"
  setNegative                               False = write "\ESC[27m"
  setDefault                                      = write "\ESC[m"
  setForeground c@T.ColorDefault                  = write "\ESC[39m"
  setForeground c@(T.Color4Bit T.Black     False) = write "\ESC[30m"
  setForeground c@(T.Color4Bit T.Red       False) = write "\ESC[31m"
  setForeground c@(T.Color4Bit T.Green     False) = write "\ESC[32m"
  setForeground c@(T.Color4Bit T.Yellow    False) = write "\ESC[33m"
  setForeground c@(T.Color4Bit T.Blue      False) = write "\ESC[34m"
  setForeground c@(T.Color4Bit T.Magenta   False) = write "\ESC[35m"
  setForeground c@(T.Color4Bit T.Cyan      False) = write "\ESC[36m"
  setForeground c@(T.Color4Bit T.White     False) = write "\ESC[37m"
  setForeground c@(T.Color4Bit T.Black      True) = write "\ESC[90m"
  setForeground c@(T.Color4Bit T.Red        True) = write "\ESC[91m"
  setForeground c@(T.Color4Bit T.Green      True) = write "\ESC[92m"
  setForeground c@(T.Color4Bit T.Yellow     True) = write "\ESC[93m"
  setForeground c@(T.Color4Bit T.Blue       True) = write "\ESC[94m"
  setForeground c@(T.Color4Bit T.Magenta    True) = write "\ESC[95m"
  setForeground c@(T.Color4Bit T.Cyan       True) = write "\ESC[96m"
  setForeground c@(T.Color4Bit T.White      True) = write "\ESC[97m"
  setForeground _                                 = error "FIXME"
  setBackground c@T.ColorDefault                  = write "\ESC[49m"
  setBackground c@(T.Color4Bit T.Black     False) = write "\ESC[40m"
  setBackground c@(T.Color4Bit T.Red       False) = write "\ESC[41m"
  setBackground c@(T.Color4Bit T.Green     False) = write "\ESC[42m"
  setBackground c@(T.Color4Bit T.Yellow    False) = write "\ESC[43m"
  setBackground c@(T.Color4Bit T.Blue      False) = write "\ESC[44m"
  setBackground c@(T.Color4Bit T.Magenta   False) = write "\ESC[45m"
  setBackground c@(T.Color4Bit T.Cyan      False) = write "\ESC[46m"
  setBackground c@(T.Color4Bit T.White     False) = write "\ESC[47m"
  setBackground c@(T.Color4Bit T.Black      True) = write "\ESC[100m"
  setBackground c@(T.Color4Bit T.Red        True) = write "\ESC[101m"
  setBackground c@(T.Color4Bit T.Green      True) = write "\ESC[102m"
  setBackground c@(T.Color4Bit T.Yellow     True) = write "\ESC[103m"
  setBackground c@(T.Color4Bit T.Blue       True) = write "\ESC[104m"
  setBackground c@(T.Color4Bit T.Magenta    True) = write "\ESC[105m"
  setBackground c@(T.Color4Bit T.Cyan       True) = write "\ESC[106m"
  setBackground c@(T.Color4Bit T.White      True) = write "\ESC[107m"
  setBackground _                                 = error "FIXME"

instance (MonadIO m) => T.MonadScreen (RemoteTerminalT m) where
  clear                                           = write "\ESC[H"
  cursorUp i                                      = write $ "\ESC[" <> Text.pack (show i) <> "A"
  cursorDown i                                    = write $ "\ESC[" <> Text.pack (show i) <> "B"
  cursorForward i                                 = write $ "\ESC[" <> Text.pack (show i) <> "C"
  cursorBackward i                                = write $ "\ESC[" <> Text.pack (show i) <> "D"
  cursorPosition x y                              = write $ "\ESC[" <> Text.pack (show x) <> ";" <> Text.pack (show y) <> "H"
  cursorVisible                             False = write "\ESC[?25l"
  cursorVisible                              True = write "\ESC[?25h"
  --askCursorPosition                               = write "\ESC[6n"
  getCursorPosition                               = pure (0,0)
  getScreenSize                                   = pure (0,0)

write :: (MonadIO m) => Text.Text -> RemoteTerminalT m ()
write t = RemoteTerminalT $ do
  env <- ask
  liftIO $ atomically $ envOutput env t
