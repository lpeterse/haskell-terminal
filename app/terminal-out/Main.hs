{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           System.Environment

import qualified Data.Text.Prettyprint.Doc as PP

import           Control.Monad.Terminal
import           System.Terminal.Ansi

main :: IO ()
main = withTerminal $ runAnsiTerminalT foo

foo :: (MonadTerminal m, MonadIO m) => m ()
foo = putDocLn doc >> flush

doc :: (MonadFormatPrinter m, MonadColorPrinter m, Annotation m ~ ann) => PP.Doc ann
doc = mconcat
  [ PP.annotate (foreground $ bright Red) "Hallo Welt!"
  , PP.hardline
  , PP.hang 10 $ "ssdfhsjdfhksjdhfkjsdhfks" PP.<+> "hdfjkshdfkjshddh" PP.<+> "fjksdhfkshdfkjshdfjks"
            PP.<+> "hdfkjshdfjhskdjfhsjksdhfjshdfjshdkj" PP.<+> "fhsdkjfhskjdfhjksdhfjksdhfjks"
            PP.<+> "hdfkjshdfkh" PP.<+> PP.annotate bold "jdhfkjshdfkjshdfksjhdkfjhsdkjfhs" PP.<+> "dkjfhskjdhfkjshdfkjshdfj"
            PP.<+> "kshdfkjshdfkjshf"
  , PP.line
  , PP.line
  , PP.annotate (background $ dull Blue) "FOOBAR"
  ]
