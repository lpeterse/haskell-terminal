module System.Terminal.Internal (
    -- ** Terminal
      Terminal (..)
    , Command (..)
    , Attribute (..)
    , Color (..)
    , Decoder (..)
    , defaultDecoder
    , defaultEncode
      -- ** LocalTerminal
    , System.Terminal.Platform.LocalTerminal ()
      -- ** VirtualTerminal (for testing)
    , VirtualTerminal (..)
    , VirtualTerminalSettings (..)
    , withVirtualTerminal
    ) where

import System.Terminal.Decoder
import System.Terminal.Encoder
import System.Terminal.Terminal
import System.Terminal.Platform
import System.Terminal.Virtual
