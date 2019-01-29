{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Text.Prettyprint.Doc
import           System.Terminal

import           Prelude                   hiding ((<>))

main :: IO ()
main = withTerminal $ runTerminalT foo

foo :: (MonadTerminal m, MonadIO m) => m ()
foo = printer >> flush

printer :: (MonadFormattingPrinter m, MonadColorPrinter m) => m ()
printer = putDoc $ annotate (foreground $ bright blue) "This is blue!" <> line
                <> annotate bold ("Just bold!" <+> otherDoc <+> "..just bold again")
                <> doc

doc :: (MonadFormattingPrinter m, MonadColorPrinter m, Attribute m ~ ann) => Doc ann
doc = mconcat
  [ annotate (foreground red) "Hallo Welt!"
  , hardline
  , hang 10 $ "ssdfhsjdfhksjdhfkjsdhfks" <+> "hdfjkshdfkjshddh" <+> "fjksdhfkshdfkjshdfjks"
            <+> "hdfkjshdfjhskdjfhsjksdhfjshdfjshdkj" <+> "fhsdkjfhskjdfhjksdhfjksdhfjks"
            <+> "hdfkjshdfkh" <+> annotate bold "jdhfkjshdfkjshdfksjhdkfjhsdkjfhs" <+> "dkjfhskjdhfkjshdfkjshdfj"
            <+> "中國哲學書電子化計劃"
            <+> "jfksdfjlksdfjls"
            <+> "\x1d11e"
  , line
  , line
  , annotate (background blue) "FOOBAR"
  ]

otherDoc :: (MonadColorPrinter m, Attribute m ~ ann) => Doc ann
otherDoc = annotate (background red) "BOLD ON RED BACKGROUND"
