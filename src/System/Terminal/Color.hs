module System.Terminal.Color where

import           Data.Word

data Color
  = ColorDefault
  | Color4Bit  Color3Bit Bool
  | Color8Bit  Word8
  | Color24bit Word8 Word8 Word8
  deriving (Eq, Ord, Show)

data Color3Bit
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Eq, Ord, Show)

def       = ColorDefault

red       = Color4Bit Red False
redBright = Color4Bit Red True

rgb :: Word8 -> Word8 -> Word8 -> Color
rgb  = Color24bit
