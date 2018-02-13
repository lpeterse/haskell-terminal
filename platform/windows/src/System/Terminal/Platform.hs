module System.Terminal.Platform
  ( withTerminal
  , gatherInputEvents
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad               (forever, void)
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.State
import qualified Data.ByteString             as BS

import qualified System.Terminal.Ansi        as T
import qualified System.Terminal.Events      as T

withTerminal :: IO a -> IO a
withTerminal = withTerminalInput . withTerminalOutput
  where
    withTerminalInput = bracket
      ( getConsoleInputModeDesired >>= setConsoleInputMode )
      ( setConsoleInputMode ) . const
    withTerminalOutput = bracket
      ( getConsoleOutputModeDesired >>= setConsoleOutputMode )
      ( setConsoleOutputMode ) . const

gatherInputEvents :: (T.Event -> STM ()) -> IO ()
gatherInputEvents push = flip evalStateT BS.empty $ forever $ do
  ev <- T.decodeAnsi
  liftIO $ atomically $ push (mapEvent ev)
  where
    mapEvent (T.EvKey (T.KChar 'M') [T.MCtrl]) = T.EvKey T.KEnter []
    mapEvent (T.EvKey (T.KChar '\DEL') [])     = T.EvKey (T.KBackspace 1) []
    mapEvent ev                                = ev

foreign import ccall unsafe "hs_get_console_input_mode_desired"
  getConsoleInputModeDesired :: IO Int

foreign import ccall unsafe "hs_set_console_input_mode"
  setConsoleInputMode       :: Int -> IO Int

foreign import ccall unsafe "hs_get_console_output_mode_desired"
  getConsoleOutputModeDesired :: IO Int

foreign import ccall unsafe "hs_set_console_output_mode"
  setConsoleOutputMode       :: Int -> IO Int
