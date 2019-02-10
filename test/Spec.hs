module Main where

import           Data.Monoid

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Spec.Decoder
import qualified Spec.Virtual

main :: IO ()
main = defaultMain $ testGroup "Control.Monad.Terminal"
  [ Spec.Decoder.tests
  , Spec.Virtual.tests
  ]
