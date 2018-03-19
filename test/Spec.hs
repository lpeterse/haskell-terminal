{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid

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
  [ testCase "NUL character" $ f "\NUL" [[]]
  , testCase "SOH character" $ f "\SOH" [[KeyEvent (CharKey 'A') ctrlKey]]
  , testCase "STX character" $ f "\STX" [[KeyEvent (CharKey 'B') ctrlKey]]
  , testCase "ETX character" $ f "\ETX" [[KeyEvent (CharKey 'C') ctrlKey]]
  , testCase "EOT character" $ f "\EOT" [[KeyEvent (CharKey 'D') ctrlKey]]
  , testCase "ENQ character" $ f "\ENQ" [[KeyEvent (CharKey 'E') ctrlKey]]
  , testCase "ACK character" $ f "\ACK" [[KeyEvent (CharKey 'F') ctrlKey]]
  , testCase "\\a character" $ f "\a"   [[KeyEvent (CharKey 'G') ctrlKey]]
  , testCase "\\b character" $ f "\b"   [[KeyEvent (CharKey 'H') ctrlKey, KeyEvent DeleteKey mempty]]
  , testCase "\\t character" $ f "\t"   [[KeyEvent (CharKey 'I') ctrlKey, KeyEvent TabKey mempty]]
  , testCase "\\n character" $ f "\n"   [[KeyEvent (CharKey 'J') ctrlKey, KeyEvent EnterKey mempty]]
  , testCase "\\v character" $ f "\v"   [[KeyEvent (CharKey 'K') ctrlKey]]
  , testCase "\\f character" $ f "\f"   [[KeyEvent (CharKey 'L') ctrlKey]]
  , testCase "\\r character" $ f "\r"   [[KeyEvent (CharKey 'M') ctrlKey]]
  , testCase "SO  character" $ f "\SO"  [[KeyEvent (CharKey 'N') ctrlKey]]
  , testCase "SI  character" $ f "\SI"  [[KeyEvent (CharKey 'O') ctrlKey]]
  , testCase "DLE character" $ f "\DLE" [[KeyEvent (CharKey 'P') ctrlKey]]
  , testCase "DC1 character" $ f "\DC1" [[KeyEvent (CharKey 'Q') ctrlKey]]
  , testCase "DC2 character" $ f "\DC2" [[KeyEvent (CharKey 'R') ctrlKey]]
  , testCase "DC3 character" $ f "\DC3" [[KeyEvent (CharKey 'S') ctrlKey]]
  , testCase "DC4 character" $ f "\DC4" [[KeyEvent (CharKey 'T') ctrlKey]]
  , testCase "NAK character" $ f "\NAK" [[KeyEvent (CharKey 'U') ctrlKey]]
  , testCase "SYN character" $ f "\SYN" [[KeyEvent (CharKey 'V') ctrlKey]]
  , testCase "ETB character" $ f "\ETB" [[KeyEvent (CharKey 'W') ctrlKey]]
  , testCase "CAN character" $ f "\CAN" [[KeyEvent (CharKey 'X') ctrlKey]]
  , testCase "EM  character" $ f "\EM"  [[KeyEvent (CharKey 'Y') ctrlKey]]
  , testCase "SUB character" $ f "\SUB" [[KeyEvent (CharKey 'Z') ctrlKey]]
  , testCase "ESC character" $ f "\ESC" [[]]
  , testCase "ESC character + NUL" $ f "\ESC\NUL" [[],[KeyEvent (CharKey '[') ctrlKey, KeyEvent EscapeKey mempty]]
  , testCase "FS  character" $ f "\FS"  [[KeyEvent (CharKey '\\') ctrlKey]]
  , testCase "GS  character" $ f "\GS"  [[KeyEvent (CharKey ']') ctrlKey]]
  , testCase "RS  character" $ f "\RS"  [[KeyEvent (CharKey '^') ctrlKey]]
  , testCase "US  character" $ f "\US"  [[KeyEvent (CharKey '_') ctrlKey]]
  , testCase "space character" $ f "\SP" [[KeyEvent SpaceKey mempty]]
  , testCase "single ASCII character" $ f "a" [[KeyEvent (CharKey 'a') mempty]]
  ]
  where
    f = assertDecoding $ \case
      '\t'   -> Just $ KeyEvent TabKey mempty
      '\n'   -> Just $ KeyEvent EnterKey mempty
      '\b'   -> Just $ KeyEvent DeleteKey mempty
      '\DEL' -> Just $ KeyEvent BackspaceKey mempty
      _      -> Nothing

-- | Only change these tests after having validated the behavior
--   with the actual terminal emulator! This is the primary reason
--   for having duplicate tests for different terminal emulators.
testDecoderWindowsConsole :: TestTree
testDecoderWindowsConsole = testGroup "Windows Console"
  [ testCase "tab key"         $ f "\r"        [[KeyEvent (CharKey 'M') ctrlKey, KeyEvent EnterKey mempty]]
  , testCase "delete key"      $ f "\ESC[3~"   [[],[],[],[KeyEvent DeleteKey mempty]]
  , testCase "function key 1"  $ f "\ESCOP"    [[],[],[KeyEvent (FunctionKey 1) mempty]]
  , testCase "function key 2"  $ f "\ESCOQ"    [[],[],[KeyEvent (FunctionKey 2) mempty]]
  , testCase "function key 3"  $ f "\ESCOR"    [[],[],[KeyEvent (FunctionKey 3) mempty]]
  , testCase "function key 4"  $ f "\ESCOS"    [[],[],[KeyEvent (FunctionKey 4) mempty]]
  , testCase "function key 5"  $ f "\ESC[15~"  [[],[],[],[],[KeyEvent (FunctionKey 5) mempty]]
  , testCase "function key 6"  $ f "\ESC[17~"  [[],[],[],[],[KeyEvent (FunctionKey 6) mempty]]
  , testCase "function key 7"  $ f "\ESC[18~"  [[],[],[],[],[KeyEvent (FunctionKey 7) mempty]]
  , testCase "function key 8"  $ f "\ESC[19~"  [[],[],[],[],[KeyEvent (FunctionKey 8) mempty]]
  , testCase "function key 9"  $ f "\ESC[20~"  [[],[],[],[],[KeyEvent (FunctionKey 9) mempty]]
  , testCase "function key 10" $ f "\ESC[21~"  [[],[],[],[],[KeyEvent (FunctionKey 10) mempty]]
  , testCase "function key 11" $ f "\ESC[23~"  [[],[],[],[],[KeyEvent (FunctionKey 11) mempty]]
  , testCase "function key 12" $ f "\ESC[24~"  [[],[],[],[],[KeyEvent (FunctionKey 12) mempty]]
  , testCase "function key 12, shift" $ f "\ESC[24;2~"  [[],[],[],[],[],[],[KeyEvent (FunctionKey 12) shiftKey]]
  , testCase "function key 12, alt" $ f "\ESC[24;3~"  [[],[],[],[],[],[],[KeyEvent (FunctionKey 12) altKey]]
  , testCase "function key 12, shift+alt" $ f "\ESC[24;4~"  [[],[],[],[],[],[],[KeyEvent (FunctionKey 12) (shiftKey <> altKey)]]
  , testCase "function key 12, ctrl" $ f "\ESC[24;5~"  [[],[],[],[],[],[],[KeyEvent (FunctionKey 12) ctrlKey]]
  , testCase "function key 12, shift+ctrl" $ f "\ESC[24;6~"  [[],[],[],[],[],[],[KeyEvent (FunctionKey 12) (shiftKey <> ctrlKey)]]
  , testCase "function key 12, alt+ctrl" $ f "\ESC[24;7~"  [[],[],[],[],[],[],[KeyEvent (FunctionKey 12) (altKey <> ctrlKey)]]
  , testCase "function key 12, shift+alt+ctrl" $ f "\ESC[24;8~"  [[],[],[],[],[],[],[KeyEvent (FunctionKey 12) (shiftKey <> altKey <> ctrlKey)]]
  ]
  where
    -- The special chars are assumed to be constant on Windows.
    f = assertDecoding $ \case
      '\t'   -> Just $ KeyEvent TabKey mempty
      '\r'   -> Just $ KeyEvent EnterKey mempty
      '\DEL' -> Just $ KeyEvent BackspaceKey mempty
      _      -> Nothing

-- | Only change these tests after having validated the behavior
--   with the actual terminal emulator! This is the primary reason
--   for having duplicate tests for different terminal emulators.
testDecoderXterm :: TestTree
testDecoderXterm = testGroup "Xterm"
  []

-- | Only change these tests after having validated the behavior
--   with the actual terminal emulator! This is the primary reason
--   for having duplicate tests for different terminal emulators.
testDecoderGnomeTerminal :: TestTree
testDecoderGnomeTerminal = testGroup "Gnome Terminal"
  []

-- | Only change these tests after having validated the behavior
--   with the actual terminal emulator! This is the primary reason
--   for having duplicate tests for different terminal emulators.
testDecoderRxvtUnicode :: TestTree
testDecoderRxvtUnicode = testGroup "Rxvt Unicode"
  []

assertDecoding :: (Char -> Maybe Event) -> String -> [[Event]] -> Assertion
assertDecoding specialChars input expected
  = expected @=? decode (ansiDecoder specialChars) input
  where
    decode :: Decoder -> String -> [[Event]]
    decode decoder = \case
      [] -> []
      (x:xs) ->  let (events, decoder') = feedDecoder decoder x
                 in events : decode decoder' xs
