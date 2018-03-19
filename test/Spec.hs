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
  [ testCase "NUL is skipped" $ f "\NUL" [[]]
  , testCase "SOH is ctrl+A" $ f "\SOH" [[KeyEvent (CharKey 'A') ctrlKey]]
  , testCase "STX is ctrl+B" $ f "\STX" [[KeyEvent (CharKey 'B') ctrlKey]]
  , testCase "ETX is ctrl+C" $ f "\ETX" [[KeyEvent (CharKey 'C') ctrlKey]]
  , testCase "EOT is ctrl+D" $ f "\EOT" [[KeyEvent (CharKey 'D') ctrlKey]]
  , testCase "ENQ is ctrl+E" $ f "\ENQ" [[KeyEvent (CharKey 'E') ctrlKey]]
  , testCase "ACK is ctrl+F" $ f "\ACK" [[KeyEvent (CharKey 'F') ctrlKey]]
  , testCase "\\a is ctrl+G" $ f "\a"   [[KeyEvent (CharKey 'G') ctrlKey]]
  , testCase "\\b is ctrl+H" $ f "\b"   [[KeyEvent (CharKey 'H') ctrlKey]]
  , testCase "\\t is ctrl+I" $ f "\t"   [[KeyEvent (CharKey 'I') ctrlKey]]
  , testCase "\\n is ctrl+J" $ f "\n"   [[KeyEvent (CharKey 'J') ctrlKey]]
  , testCase "\\v is ctrl+K" $ f "\v"   [[KeyEvent (CharKey 'K') ctrlKey]]
  , testCase "\\f is ctrl+L" $ f "\f"   [[KeyEvent (CharKey 'L') ctrlKey]]
  , testCase "\\r is ctrl+M" $ f "\r"   [[KeyEvent (CharKey 'M') ctrlKey]]
  , testCase "SO  is ctrl+N" $ f "\SO"  [[KeyEvent (CharKey 'N') ctrlKey]]
  , testCase "SI  is ctrl+O" $ f "\SI"  [[KeyEvent (CharKey 'O') ctrlKey]]
  , testCase "DLE is ctrl+P" $ f "\DLE" [[KeyEvent (CharKey 'P') ctrlKey]]
  , testCase "DC1 is ctrl+Q" $ f "\DC1" [[KeyEvent (CharKey 'Q') ctrlKey]]
  , testCase "DC2 is ctrl+R" $ f "\DC2" [[KeyEvent (CharKey 'R') ctrlKey]]
  , testCase "DC3 is ctrl+S" $ f "\DC3" [[KeyEvent (CharKey 'S') ctrlKey]]
  , testCase "DC4 is ctrl+T" $ f "\DC4" [[KeyEvent (CharKey 'T') ctrlKey]]
  , testCase "NAK is ctrl+U" $ f "\NAK" [[KeyEvent (CharKey 'U') ctrlKey]]
  , testCase "SYN is ctrl+V" $ f "\SYN" [[KeyEvent (CharKey 'V') ctrlKey]]
  , testCase "ETB is ctrl+W" $ f "\ETB" [[KeyEvent (CharKey 'W') ctrlKey]]
  , testCase "CAN is ctrl+X" $ f "\CAN" [[KeyEvent (CharKey 'X') ctrlKey]]
  , testCase "EM  is ctrl+Y" $ f "\EM"  [[KeyEvent (CharKey 'Y') ctrlKey]]
  , testCase "SUB is ctrl+Z" $ f "\SUB" [[KeyEvent (CharKey 'Z') ctrlKey]]
  , testCase "ESC cannot be decided" $ f "\ESC" [[]]
  , testCase "ESC+NUL is ctrl+[ and escape key" $ f "\ESC\NUL" [[],[KeyEvent (CharKey '[') ctrlKey, KeyEvent EscapeKey mempty]]
  , testCase "FS  is ctrl+\\" $ f "\FS"  [[KeyEvent (CharKey '\\') ctrlKey]]
  , testCase "GS  is ctrl+]" $ f "\GS"  [[KeyEvent (CharKey ']') ctrlKey]]
  , testCase "RS  is ctrl+^" $ f "\RS"  [[KeyEvent (CharKey '^') ctrlKey]]
  , testCase "US  is ctrl+_" $ f "\US"  [[KeyEvent (CharKey '_') ctrlKey]]
  , testCase "SP  is space key" $ f "\SP" [[KeyEvent SpaceKey mempty]]
  , testCase "'a' is character key 'a''" $ f "a" [[KeyEvent (CharKey 'a') mempty]]
  ]
  where
    f = assertDecoding (const Nothing)

-- | Only change these tests after having validated the behavior
--   with the actual terminal emulator! This is the primary reason
--   for having duplicate tests for different terminal emulators.
testDecoderWindowsConsole :: TestTree
testDecoderWindowsConsole = testGroup "Windows Console"
  [ testCase "tab key"         $ f "\t"        [[KeyEvent (CharKey 'I') ctrlKey, KeyEvent TabKey mempty]]
  , testCase "enter key"       $ f "\r"        [[KeyEvent (CharKey 'M') ctrlKey, KeyEvent EnterKey mempty]]
  , testCase "enter key (when pressed with ctrl)" $ f "\n" [[KeyEvent (CharKey 'J') ctrlKey, KeyEvent EnterKey mempty]]
  , testCase "delete key"      $ f "\ESC[3~"   [[],[],[],[KeyEvent DeleteKey mempty]]
  , testCase "backspace key"   $ f "\DEL"      [[KeyEvent (CharKey '?') ctrlKey, KeyEvent BackspaceKey mempty]]
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
      '\n'   -> Just $ KeyEvent EnterKey mempty
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
    -- The decoder function shall be tested with empty modifiers.
    -- Its implementation is assumed to just monotonically add the supplied modifiers
    -- to all emitted events. This is hardly prone to errors and it is not tested.
    mods :: Modifiers
    mods = mempty
    decode :: Decoder -> String -> [[Event]]
    decode decoder = \case
      [] -> []
      (x:xs) ->  let (events, decoder') = feedDecoder decoder mods x
                 in events : decode decoder' xs
