module Main where

import           Control.Monad

import qualified System.Terminal as T

main :: IO ()
main = T.runAnsiTerminalT $ forever $
  T.getEvent >>= T.putStringLn . show
