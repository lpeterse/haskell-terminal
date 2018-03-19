{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Monad.Terminal

main :: IO ()
main = defaultMain $ testGroup "Control.Monad.Terminal"
  [ testDecoder
  ]

testDecoder :: TestTree
testDecoder = testGroup "ansiDecoder"
  [ testDecoderGeneric
  , testDecoderWindowsConsole
  , testDecoderXterm
  , testDecoderGnomeTerminal
  , testDecoderRxvtUnicode
  ]

testDecoderGeneric :: TestTree
testDecoderGeneric = testGroup "Generic Ansi"
  [ testCase "NUL character" $ assertDecoding "\NUL" [[]]
  , testCase "SOH character" $ assertDecoding "\SOH" [[KeyEvent (CharKey 'A') ctrlKey]]
  , testCase "STX character" $ assertDecoding "\STX" [[KeyEvent (CharKey 'B') ctrlKey]]
  , testCase "ETX character" $ assertDecoding "\ETX" [[KeyEvent (CharKey 'C') ctrlKey]]
  , testCase "EOT character" $ assertDecoding "\EOT" [[KeyEvent (CharKey 'D') ctrlKey]]
  , testCase "ENQ character" $ assertDecoding "\ENQ" [[KeyEvent (CharKey 'E') ctrlKey]]
  , testCase "ACK character" $ assertDecoding "\ACK" [[KeyEvent (CharKey 'F') ctrlKey]]
  , testCase "\\a character" $ assertDecoding "\a"   [[KeyEvent (CharKey 'G') ctrlKey]]
  , testCase "\\b character" $ assertDecoding "\b"   [[KeyEvent (CharKey 'H') ctrlKey]]
  , testCase "\\t character" $ assertDecoding "\t"   [[KeyEvent (CharKey 'I') ctrlKey]]
  , testCase "\\n character" $ assertDecoding "\n"   [[KeyEvent (CharKey 'J') ctrlKey, KeyEvent EnterKey mempty]]
  , testCase "\\v character" $ assertDecoding "\v"   [[KeyEvent (CharKey 'K') ctrlKey]]
  , testCase "\\f character" $ assertDecoding "\f"   [[KeyEvent (CharKey 'L') ctrlKey]]
  , testCase "\\r character" $ assertDecoding "\r"   [[KeyEvent (CharKey 'M') ctrlKey]]
  , testCase "SO  character" $ assertDecoding "\SO"  [[KeyEvent (CharKey 'N') ctrlKey]]
  , testCase "SI  character" $ assertDecoding "\SI"  [[KeyEvent (CharKey 'O') ctrlKey]]
  , testCase "DLE character" $ assertDecoding "\DLE" [[KeyEvent (CharKey 'P') ctrlKey]]
  , testCase "DC1 character" $ assertDecoding "\DC1" [[KeyEvent (CharKey 'Q') ctrlKey]]
  , testCase "DC2 character" $ assertDecoding "\DC2" [[KeyEvent (CharKey 'R') ctrlKey]]
  , testCase "DC3 character" $ assertDecoding "\DC3" [[KeyEvent (CharKey 'S') ctrlKey]]
  , testCase "DC4 character" $ assertDecoding "\DC4" [[KeyEvent (CharKey 'T') ctrlKey]]
  , testCase "NAK character" $ assertDecoding "\NAK" [[KeyEvent (CharKey 'U') ctrlKey]]
  , testCase "SYN character" $ assertDecoding "\SYN" [[KeyEvent (CharKey 'V') ctrlKey]]
  , testCase "ETB character" $ assertDecoding "\ETB" [[KeyEvent (CharKey 'W') ctrlKey]]
  , testCase "CAN character" $ assertDecoding "\CAN" [[KeyEvent (CharKey 'X') ctrlKey]]
  , testCase "EM  character" $ assertDecoding "\EM"  [[KeyEvent (CharKey 'Y') ctrlKey]]
  , testCase "SUB character" $ assertDecoding "\SUB" [[KeyEvent (CharKey 'Z') ctrlKey]]
  , testCase "ESC character" $ assertDecoding "\ESC" [[]]
  , testCase "FS  character" $ assertDecoding "\FS"  [[KeyEvent (CharKey '\\') ctrlKey]]
  , testCase "GS  character" $ assertDecoding "\GS"  [[KeyEvent (CharKey ']') ctrlKey]]
  , testCase "RS  character" $ assertDecoding "\RS"  [[KeyEvent (CharKey '^') ctrlKey]]
  , testCase "US  character" $ assertDecoding "\US"  [[KeyEvent (CharKey '_') ctrlKey]]
  , testCase "space character" $ assertDecoding "\SP" [[KeyEvent (CharKey ' ') mempty]]
  , testCase "single ASCII character" $ assertDecoding "a" [[KeyEvent (CharKey 'a') mempty]]
  ]

testDecoderWindowsConsole :: TestTree
testDecoderWindowsConsole = testGroup "Windows Console"
  []

testDecoderXterm :: TestTree
testDecoderXterm = testGroup "Xterm"
  []

testDecoderGnomeTerminal :: TestTree
testDecoderGnomeTerminal = testGroup "Gnome Terminal"
  []

testDecoderRxvtUnicode :: TestTree
testDecoderRxvtUnicode = testGroup "Rxvt Unicode"
  []

assertDecoding :: String -> [[Event]] -> Assertion
assertDecoding input expected
  = expected @=? decode ansiDecoder input
  where
    decode :: Decoder -> String -> [[Event]]
    decode decoder = \case
      [] -> []
      (x:xs) ->  let (events, decoder') = feedDecoder decoder x
                 in events : decode decoder' xs
