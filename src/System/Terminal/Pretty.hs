{-# LANGUAGE LambdaCase #-}
module System.Terminal.Pretty
  ( -- * Pretty Printing
    Doc ()
  , putDoc
  , putDocLn
    -- * Colors
    -- ** color
  , color
    -- ** onColor
  , onColor
  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  ) where

import           Data.Semigroup
import           Data.String
import qualified Data.Text             as Text

import           System.Terminal.Class
import           System.Terminal.Color

data Doc
  = Empty
  | Append Doc Doc
  | Char Char
  | String String
  | Text Text.Text
  | ColoredForeground Color Doc
  | ColoredBackground Color Doc
  | Underlined Doc
  | Italic Doc
  | Bold Doc
  | Line
  deriving (Eq, Ord, Show)

instance IsString Doc where
  fromString = String

instance Monoid Doc where
  mempty  = empty
  mappend = append

instance Semigroup Doc where
  (<>) = append

empty :: Doc
empty  = Empty

append :: Doc -> Doc -> Doc
append  = Append

line :: Doc
line  = Line

putDocLn :: (MonadColorPrinter m, MonadIsolate m) => Doc -> m ()
putDocLn doc = putDoc $ doc <> line

putDoc :: (MonadColorPrinter m, MonadIsolate m) => Doc -> m ()
putDoc = \case
  Empty -> pure ()
  Append a b -> putDoc a >> putDoc b
  Char c -> putString [c]
  String s -> putString s
  ColoredForeground color doc -> isolate $ do
    setForegroundColor color
    putDoc doc
  ColoredBackground color doc -> isolate $ do
    setBackgroundColor color
    putDoc doc
  Underlined doc -> isolate $ do
    setUnderline True
    putDoc doc
  Italic doc -> putDoc doc
  Bold doc -> putDoc doc
  Line -> putLn

-------------------------------------------------------------------------------
-- Colors
-------------------------------------------------------------------------------

color   :: Color -> Doc -> Doc
color    = ColoredForeground

onColor :: Color -> Doc -> Doc
onColor  = ColoredBackground
