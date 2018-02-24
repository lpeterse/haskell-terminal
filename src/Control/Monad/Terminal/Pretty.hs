{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Terminal.Pretty
  ( -- * Pretty Printing
    -- * Formatting
    -- ** inverted
    inverted
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
import qualified Data.Text                                    as Text
import qualified Data.Text.Prettyprint.Doc                    as PP

import qualified Control.Monad.Terminal                       as T
import           Control.Monad.Terminal.Ansi.Color
import qualified Control.Monad.Terminal.Ansi.MonadAnsiPrinter as T

bold :: T.MonadAnsiPrinter m => PP.Doc (T.Annotation m) -> PP.Doc (T.Annotation m)
bold = PP.annotate (T.bold True)

inverted :: T.MonadAnsiPrinter m => PP.Doc (T.Annotation m) -> PP.Doc (T.Annotation m)
inverted = PP.annotate (T.inverted True)

underlined :: T.MonadAnsiPrinter m => PP.Doc (T.Annotation m) -> PP.Doc (T.Annotation m)
underlined = PP.annotate (T.underlined True)

color   :: T.MonadAnsiPrinter m => Color -> PP.Doc (T.Annotation m) -> PP.Doc (T.Annotation m)
color c = PP.annotate (T.foreground c)

onColor :: T.MonadAnsiPrinter m => Color -> PP.Doc (T.Annotation m) -> PP.Doc (T.Annotation m)
onColor c  = PP.annotate (T.background c)
