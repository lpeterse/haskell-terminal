{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Monad.Terminal
import           Data.Text.Prettyprint.Doc
import           System.Terminal

import           Prelude                   hiding ((<>))

main :: IO ()
main = withTerminal $ runAnsiTerminalT foo

foo :: (MonadTerminal m, MonadIO m) => m ()
foo = printer >> flush

printer :: (MonadFormatPrinter m, MonadColorPrinter m) => m ()
printer = putDoc $ annotate (foreground $ bright Blue) "This is blue!" <> line
                <> annotate bold ("Just bold!" <+> otherDoc <+> "..just bold again")

doc :: (MonadFormatPrinter m, MonadColorPrinter m, Annotation m ~ ann) => Doc ann
doc = mconcat
  [ annotate (foreground $ bright Red) "Hallo Welt!"
  , hardline
  , hang 10 $ "ssdfhsjdfhksjdhfkjsdhfks" <+> "hdfjkshdfkjshddh" <+> "fjksdhfkshdfkjshdfjks"
            <+> "hdfkjshdfjhskdjfhsjksdhfjshdfjshdkj" <+> "fhsdkjfhskjdfhjksdhfjksdhfjks"
            <+> "hdfkjshdfkh" <+> annotate bold "jdhfkjshdfkjshdfksjhdkfjhsdkjfhs" <+> "dkjfhskjdhfkjshdfkjshdfj"
            <+> "kshdfkjshdfkjshf"
  , line
  , line
  , annotate (background $ dull Blue) "FOOBAR"
  ]

otherDoc :: (MonadColorPrinter m, Annotation m ~ ann) => Doc ann
otherDoc = annotate (background $ dull Red) "BOLD ON RED BACKGROUND"
