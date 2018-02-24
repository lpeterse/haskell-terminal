{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           System.Environment

import qualified Data.Text.Prettyprint.Doc     as PP

import qualified Control.Monad.Terminal        as T
import qualified Control.Monad.Terminal.Color  as T
import qualified Control.Monad.Terminal.Events as T
import qualified Control.Monad.Terminal.Pretty as T

import qualified System.Terminal.Ansi          as T

main :: IO ()
main = T.withStandardTerminal $ T.runAnsiTerminalT foo

foo :: (T.MonadTerminal m, MonadIO m) => m ()
foo = T.putDocLn doc >> T.flush

doc :: PP.Doc ann
doc = mconcat
  [ --T.color T.red "Hallo Welt!"
    PP.hardline
  , PP.indent 10 $ "ssdfhsjdfhksjdhfkjsdhfks" PP.<+> "hdfjkshdfkjshddh" PP.<+> "fjksdhfkshdfkjshdfjks"
            PP.<+> "hdfkjshdfjhskdjfhsjksdhfjshdfjshdkj" PP.<+> "fhsdkjfhskjdfhjksdhfjksdhfjks"
            PP.<+> "hdfkjshdfkh" PP.<+> "jdhfkjshdfkjshdfksjhdkfjhsdkjfhs" PP.<+> "dkjfhskjdhfkjshdfkjshdfj"
            PP.<+> "kshdfkjshdfkjshf"
  , PP.line
  , PP.line
  -- , T.onColor T.blue "FOOBAR"
  ]


