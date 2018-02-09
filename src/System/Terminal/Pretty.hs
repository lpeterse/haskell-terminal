module System.Terminal.Pretty where

import           Data.String

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

red       = Color Red False
redBright = Color Red True

data Doc
  = Empty
  | Char Char
  | String String
  | Colored Color Doc
  | Underlined Doc
  | Italic Doc
  | Bold Doc
  deriving (Eq, Ord, Show)

instance IsString Doc where
  fromString = String
