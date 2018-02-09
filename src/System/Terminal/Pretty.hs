module System.Terminal.Pretty where

import qualified Data.Semigroup        as S
import           Data.String
import qualified Data.Text             as Text

import           System.Terminal.Color

data Doc
  = Empty
  | Beside Doc Doc
  | Char Char
  | String String
  | Text Text.Text
  | Colored Color Doc
  | Underlined Doc
  | Italic Doc
  | Bold Doc
  deriving (Eq, Ord, Show)

instance IsString Doc where
  fromString = String

instance Monoid Doc where
  mempty  = empty
  mappend = beside

instance S.Semigroup Doc where
  (<>) = beside

empty :: Doc
empty = Empty

beside :: Doc -> Doc -> Doc
beside = Beside
