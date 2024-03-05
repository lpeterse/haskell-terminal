{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

import           Prettyprinter 
import           System.Terminal

import           Prelude                   hiding (putString)

main :: IO ()
main = withTerminal $ runTerminalT do
    putString "ABC"
    putLn
    putString "DEF"
    putLn
    flush
    liftIO (threadDelay 5000000)
    ev <- awaitEvent
    putPrettyLn (show ev)
    flush