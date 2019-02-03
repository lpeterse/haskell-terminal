module System.Terminal.Virtual where

import           Control.Monad.STM
import qualified Data.ByteString   as BS
import           Control.Monad.IO.Class

import           System.Terminal.MonadInput
import           System.Terminal.Terminal

data VirtualTerminal
    = VirtualTerminal
    { virtualSettings          :: VirtualTerminalSettings
    , virtualCursor            :: (Row, Col)
    , virtualScreen            :: [String]
    }

data VirtualTerminalSettings
    = VirtualTerminalSettings
    { virtualType              :: BS.ByteString
    , virtualWindowSize        :: (Rows,Cols)
    , virtualEvent             :: STM Event
    , virtualInterrupt         :: STM ()
    }

instance Terminal VirtualTerminal where
    termType              = virtualType      . virtualSettings
    termEvent             = virtualEvent     . virtualSettings
    termInterrupt         = virtualInterrupt . virtualSettings
    termCommand           = undefined
    termFlush _           = pure ()
    termGetWindowSize     = pure . virtualWindowSize . virtualSettings
    termGetCursorPosition = pure . virtualCursor

withVirtualTerminal :: (MonadIO m) => VirtualTerminalSettings -> (VirtualTerminal -> m a) -> m a
withVirtualTerminal settings handler = handler term
    where
        term = VirtualTerminal
            { virtualSettings          = settings :: VirtualTerminalSettings
            , virtualCursor            = (0,0)
            , virtualScreen            = replicate height (replicate width ' ')
            }
        height = fst (virtualWindowSize settings)
        width  = snd (virtualWindowSize settings)