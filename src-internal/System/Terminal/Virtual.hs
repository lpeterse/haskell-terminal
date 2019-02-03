module System.Terminal.Virtual where

import           Control.Monad.STM
import           Control.Concurrent.STM.TVar
import qualified Data.ByteString   as BS
import           Control.Monad.IO.Class

import           System.Terminal.MonadInput
import           System.Terminal.Terminal

data VirtualTerminal
    = VirtualTerminal
    { virtualSettings          :: VirtualTerminalSettings
    , virtualCursor            :: TVar (Row, Col)
    , virtualScreen            :: TVar [String]
    }

data VirtualTerminalSettings
    = VirtualTerminalSettings
    { virtualType              :: BS.ByteString
    , virtualWindowSize        :: STM (Rows,Cols)
    , virtualEvent             :: STM Event
    , virtualInterrupt         :: STM ()
    }

instance Terminal VirtualTerminal where
    termType              = virtualType      . virtualSettings
    termEvent             = virtualEvent     . virtualSettings
    termInterrupt         = virtualInterrupt . virtualSettings
    termCommand t c       = atomically (command t c) 
    termFlush _           = pure ()
    termGetWindowSize     = pure . virtualWindowSize . virtualSettings
    termGetCursorPosition = pure . virtualCursor

withVirtualTerminal :: (MonadIO m) => VirtualTerminalSettings -> (VirtualTerminal -> m a) -> m a
withVirtualTerminal settings handler = do
    term <- VirtualTerminal settings
        <$> newTVar (0,0)
        <*> newTVar (replicate height (replicate width ' '))
    handler term
    where
        height = fst (virtualWindowSize settings)
        width  = snd (virtualWindowSize settings)

command :: VirtualTerminal -> Command -> STM ()
command  = undefined