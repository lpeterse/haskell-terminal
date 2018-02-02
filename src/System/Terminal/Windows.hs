{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module System.Terminal.Windows where

import           Control.Concurrent
import           Control.Monad             (void)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Bits
import qualified Data.ByteString           as BS
import           Data.Char
import           Data.Maybe
import           Data.Word
import           System.Environment
import qualified System.IO                 as IO
import qualified System.Terminal.Events    as E
import qualified System.Terminal.Modes     as M
import           System.Terminal.Output

import           Prelude                   hiding (putStr, putStrLn)

newtype TerminalT m a
  = TerminalT (StateT BS.ByteString m a)
  deriving (Functor, Applicative, Monad, MonadIO)

runTerminalT :: (MonadMask m, MonadIO m) => TerminalT m a -> m a
runTerminalT (TerminalT m) = withVirtualTerminalMode $
  evalStateT m BS.empty

instance MonadIO m => MonadPrinter (TerminalT m) where
  putString = liftIO . IO.putStr . filter (\c-> isPrint c || isSpace c)
  putStringLn s = putString s >> nl
  nl = liftIO $ IO.putStr "\n"
  cr = liftIO $ IO.putStr "\r"
  reset = liftIO $ IO.putStr "\ESC[!p"
  flush = liftIO $ IO.hFlush IO.stdout
  setForegroundColor ColorDefault            = liftIO $ IO.putStr "\ESC[39m"
  setForegroundColor (Color Black     False) = liftIO $ IO.putStr "\ESC[30m"
  setForegroundColor (Color Red       False) = liftIO $ IO.putStr "\ESC[31m"
  setForegroundColor (Color Green     False) = liftIO $ IO.putStr "\ESC[32m"
  setForegroundColor (Color Yellow    False) = liftIO $ IO.putStr "\ESC[33m"
  setForegroundColor (Color Blue      False) = liftIO $ IO.putStr "\ESC[34m"
  setForegroundColor (Color Magenta   False) = liftIO $ IO.putStr "\ESC[35m"
  setForegroundColor (Color Cyan      False) = liftIO $ IO.putStr "\ESC[36m"
  setForegroundColor (Color White     False) = liftIO $ IO.putStr "\ESC[37m"
  setForegroundColor (Color Black      True) = liftIO $ IO.putStr "\ESC[90m"
  setForegroundColor (Color Red        True) = liftIO $ IO.putStr "\ESC[91m"
  setForegroundColor (Color Green      True) = liftIO $ IO.putStr "\ESC[92m"
  setForegroundColor (Color Yellow     True) = liftIO $ IO.putStr "\ESC[93m"
  setForegroundColor (Color Blue       True) = liftIO $ IO.putStr "\ESC[94m"
  setForegroundColor (Color Magenta    True) = liftIO $ IO.putStr "\ESC[95m"
  setForegroundColor (Color Cyan       True) = liftIO $ IO.putStr "\ESC[96m"
  setForegroundColor (Color White      True) = liftIO $ IO.putStr "\ESC[97m"
  setBackgroundColor ColorDefault            = liftIO $ IO.putStr "\ESC[49m"
  setBackgroundColor (Color Black     False) = liftIO $ IO.putStr "\ESC[40m"
  setBackgroundColor (Color Red       False) = liftIO $ IO.putStr "\ESC[41m"
  setBackgroundColor (Color Green     False) = liftIO $ IO.putStr "\ESC[42m"
  setBackgroundColor (Color Yellow    False) = liftIO $ IO.putStr "\ESC[43m"
  setBackgroundColor (Color Blue      False) = liftIO $ IO.putStr "\ESC[44m"
  setBackgroundColor (Color Magenta   False) = liftIO $ IO.putStr "\ESC[45m"
  setBackgroundColor (Color Cyan      False) = liftIO $ IO.putStr "\ESC[46m"
  setBackgroundColor (Color White     False) = liftIO $ IO.putStr "\ESC[47m"
  setBackgroundColor (Color Black      True) = liftIO $ IO.putStr "\ESC[100m"
  setBackgroundColor (Color Red        True) = liftIO $ IO.putStr "\ESC[101m"
  setBackgroundColor (Color Green      True) = liftIO $ IO.putStr "\ESC[102m"
  setBackgroundColor (Color Yellow     True) = liftIO $ IO.putStr "\ESC[103m"
  setBackgroundColor (Color Blue       True) = liftIO $ IO.putStr "\ESC[104m"
  setBackgroundColor (Color Magenta    True) = liftIO $ IO.putStr "\ESC[105m"
  setBackgroundColor (Color Cyan       True) = liftIO $ IO.putStr "\ESC[106m"
  setBackgroundColor (Color White      True) = liftIO $ IO.putStr "\ESC[107m"

instance MonadIO m => MonadScreen (TerminalT m) where

withVirtualTerminalMode :: (MonadIO m, MonadMask m) => m a -> m a
withVirtualTerminalMode = bracket before after . const
  where
    before = set (Just True)
    after = void . set
    set Nothing = pure Nothing
    set (Just x) = do
      r <- liftIO $ setVirtualTerminalMode (if x then 1 else 0)
      pure $ case r of
        0 -> Just False
        1 -> Just True
        _ -> Nothing

foreign import ccall unsafe "hs_set_vt_mode"
  setVirtualTerminalMode  :: Int -> IO Int
