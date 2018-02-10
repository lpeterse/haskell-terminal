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

rgb :: Word8 -> Word8 -> Word8 -> Color
rgb  = Color24bit

black :: Color
black = Color4Bit Black False

red :: Color
red = Color4Bit Red False

green :: Color
green = Color4Bit Green False

yellow :: Color
yellow  = Color4Bit Yellow False

blue :: Color
blue = Color4Bit Blue False

magenta :: Color
magenta = Color4Bit Magenta False

cyan :: Color
cyan = Color4Bit Cyan False

white :: Color
white = Color4Bit White False

