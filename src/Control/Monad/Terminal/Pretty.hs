{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Terminal.Pretty
  ( -- * Pretty Printing
    TermDoc
  , TermStyle (..)
  , putDoc
  , putDocLn
    -- * Formatting
    -- ** inverted
  , inverted
    -- ** underlined
  , underlined
    -- ** bold
  , bold
    -- * Colors
    -- ** color
  , color
    -- ** onColor
  , onColor
    -- ** Standard Colors
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
import qualified Data.Text                    as Text
import qualified Data.Text.Prettyprint.Doc    as PP

import qualified Control.Monad.Terminal       as T
import           Control.Monad.Terminal.Color

type TermDoc = PP.Doc TermStyle

data TermStyle
  = TermForeground Color
  | TermBackground Color
  | TermInverted
  | TermUnderlined
  | TermBold
  deriving (Eq, Ord, Show)

putDocLn :: (T.MonadColorPrinter m) => TermDoc -> m ()
putDocLn doc = putDoc $ doc <> PP.hardline

putDoc :: (T.MonadColorPrinter m) => TermDoc -> m ()
putDoc doc = do
  T.setDefault
  render [] sdoc >> T.flush
  where
    width   = PP.AvailablePerLine 20 0.4
    options = PP.defaultLayoutOptions { PP.layoutPageWidth = width }
    sdoc = PP.layoutSmart options doc
    render anns = \case
      PP.SFail           -> fail "FAIL"
      PP.SEmpty          -> pure ()
      PP.SChar c ss      -> T.putChar c >> render anns ss
      PP.SText _ t ss    -> T.putText t >> render anns ss
      PP.SLine n ss      -> T.putLn >> T.putText (Text.replicate n " ") >> render anns ss
      PP.SAnnPush ann ss -> case ann of
        TermForeground c -> T.setForeground c >> render (ann:anns) ss
        TermBackground c -> T.setBackground c >> render (ann:anns) ss
        TermInverted     -> T.setNegative True     >> render (ann:anns) ss
        TermUnderlined   -> T.setUnderline True    >> render (ann:anns) ss
        TermBold         -> T.setBold True         >> render (ann:anns) ss
      PP.SAnnPop ss      -> case anns of
        []                       -> render [] ss
        (TermForeground c:anns') -> T.setForeground ColorDefault >> render anns' ss
        (TermBackground c:anns') -> T.setBackground ColorDefault >> render anns' ss
        (TermInverted    :anns') -> T.setNegative False  >> render anns' ss
        (TermUnderlined  :anns') -> T.setUnderline False >> render anns' ss
        (TermBold        :anns') -> T.setBold False      >> render anns' ss

color   :: Color -> TermDoc -> TermDoc
color c = PP.annotate (TermForeground c)

inverted :: TermDoc -> TermDoc
inverted = PP.annotate TermInverted

underlined :: TermDoc -> TermDoc
underlined = PP.annotate TermUnderlined

bold :: TermDoc -> TermDoc
bold = PP.annotate TermBold

onColor :: Color -> TermDoc -> TermDoc
onColor c  = PP.annotate (TermBackground c)
