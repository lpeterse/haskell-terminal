module System.Terminal.Color where

data Color
  = Color Color8 Bool
  | ColorDefault
  deriving (Eq, Ord, Show)

data Color8
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

red       = Color Red False
redBright = Color Red True
