module Spec.Decoder where

import Test.Tasty
import Test.Tasty.HUnit

import System.Terminal
import System.Terminal.Internal

tests :: TestTree
tests = testGroup "System.Terminal.Decoder"
  [ testDecoderGeneric
  , testDecoderWindowsConsole
  , testDecoderXterm
  , testDecoderGnomeTerminal
  , testDecoderRxvtUnicode
  ]

testDecoderGeneric :: TestTree
testDecoderGeneric = testGroup "Generic Ansi"
  [ testCase "NUL is skipped" $ f mempty "\NUL" []
  , testCase "SOH is ctrl+A" $ f mempty "\SOH" [KeyEvent (CharKey 'A') ctrlKey]
  , testCase "STX is ctrl+B" $ f mempty "\STX" [KeyEvent (CharKey 'B') ctrlKey]
  , testCase "ETX is ctrl+C" $ f mempty "\ETX" [KeyEvent (CharKey 'C') ctrlKey]
  , testCase "EOT is ctrl+D" $ f mempty "\EOT" [KeyEvent (CharKey 'D') ctrlKey]
  , testCase "ENQ is ctrl+E" $ f mempty "\ENQ" [KeyEvent (CharKey 'E') ctrlKey]
  , testCase "ACK is ctrl+F" $ f mempty "\ACK" [KeyEvent (CharKey 'F') ctrlKey]
  , testCase "\\a is ctrl+G" $ f mempty "\a"   [KeyEvent (CharKey 'G') ctrlKey]
  , testCase "\\b is ctrl+H" $ f mempty "\b"   [KeyEvent (CharKey 'H') ctrlKey]
  , testCase "\\t is ctrl+I" $ f mempty "\t"   [KeyEvent (CharKey 'I') ctrlKey]
  , testCase "\\n is ctrl+J" $ f mempty "\n"   [KeyEvent (CharKey 'J') ctrlKey]
  , testCase "\\v is ctrl+K" $ f mempty "\v"   [KeyEvent (CharKey 'K') ctrlKey]
  , testCase "\\f is ctrl+L" $ f mempty "\f"   [KeyEvent (CharKey 'L') ctrlKey]
  , testCase "\\r is ctrl+M" $ f mempty "\r"   [KeyEvent (CharKey 'M') ctrlKey]
  , testCase "SO  is ctrl+N" $ f mempty "\SO"  [KeyEvent (CharKey 'N') ctrlKey]
  , testCase "SI  is ctrl+O" $ f mempty "\SI"  [KeyEvent (CharKey 'O') ctrlKey]
  , testCase "DLE is ctrl+P" $ f mempty "\DLE" [KeyEvent (CharKey 'P') ctrlKey]
  , testCase "DC1 is ctrl+Q" $ f mempty "\DC1" [KeyEvent (CharKey 'Q') ctrlKey]
  , testCase "DC2 is ctrl+R" $ f mempty "\DC2" [KeyEvent (CharKey 'R') ctrlKey]
  , testCase "DC3 is ctrl+S" $ f mempty "\DC3" [KeyEvent (CharKey 'S') ctrlKey]
  , testCase "DC4 is ctrl+T" $ f mempty "\DC4" [KeyEvent (CharKey 'T') ctrlKey]
  , testCase "NAK is ctrl+U" $ f mempty "\NAK" [KeyEvent (CharKey 'U') ctrlKey]
  , testCase "SYN is ctrl+V" $ f mempty "\SYN" [KeyEvent (CharKey 'V') ctrlKey]
  , testCase "ETB is ctrl+W" $ f mempty "\ETB" [KeyEvent (CharKey 'W') ctrlKey]
  , testCase "CAN is ctrl+X" $ f mempty "\CAN" [KeyEvent (CharKey 'X') ctrlKey]
  , testCase "EM  is ctrl+Y" $ f mempty "\EM"  [KeyEvent (CharKey 'Y') ctrlKey]
  , testCase "SUB is ctrl+Z" $ f mempty "\SUB" [KeyEvent (CharKey 'Z') ctrlKey]
  , testCase "ESC cannot be decided" $ f mempty "\ESC" []
  , testCase "ESC+NUL is ctrl+[ and escape key" $ f mempty "\ESC\NUL" [KeyEvent (CharKey '[') ctrlKey, KeyEvent EscapeKey mempty]
  , testCase "FS  is ctrl+\\" $ f mempty "\FS"  [KeyEvent (CharKey '\\') ctrlKey]
  , testCase "GS  is ctrl+]" $ f mempty "\GS"  [KeyEvent (CharKey ']') ctrlKey]
  , testCase "RS  is ctrl+^" $ f mempty "\RS"  [KeyEvent (CharKey '^') ctrlKey]
  , testCase "US  is ctrl+_" $ f mempty "\US"  [KeyEvent (CharKey '_') ctrlKey]
  , testCase "'a' is character key 'a''" $ f mempty "a" [KeyEvent (CharKey 'a') mempty]
  ]
  where
    f = assertDecoding (\_ _ -> Nothing)

-- | Only change these tests after having validated the behavior
--   with the actual terminal emulator! This is the primary reason
--   for having duplicate tests for different terminal emulators.
testDecoderWindowsConsole :: TestTree
testDecoderWindowsConsole = testGroup "Windows Console"
  [ testCase "tab key"                            $ f mempty "\t"          [KeyEvent (CharKey 'I') ctrlKey, KeyEvent TabKey mempty]
  , testCase "enter key"                          $ f mempty "\r"          [KeyEvent (CharKey 'M') ctrlKey, KeyEvent EnterKey mempty]
  , testCase "enter key (when pressed with ctrl)" $ f ctrlKey "\n"         [KeyEvent (CharKey 'J') ctrlKey, KeyEvent EnterKey ctrlKey]
  , testCase "delete key"                         $ f mempty "\ESC[3~"     [KeyEvent DeleteKey mempty]
  , testCase "space key"                          $ f mempty   "\SP"       [KeyEvent (CharKey ' ') mempty, KeyEvent SpaceKey mempty]
  , testCase "space key, shift"                   $ f shiftKey "\SP"       [KeyEvent (CharKey ' ') shiftKey, KeyEvent SpaceKey shiftKey]
  , testCase "backspace key"                      $ f mempty "\DEL"        [KeyEvent BackspaceKey mempty]
  , testCase "backspace key, alt"                 $ f mempty "\ESC\b\NUL"  [KeyEvent BackspaceKey altKey]
  , testCase "function key 1"                     $ f mempty "\ESCOP"      [KeyEvent (FunctionKey 1) mempty]
  , testCase "function key 2"                     $ f mempty "\ESCOQ"      [KeyEvent (FunctionKey 2) mempty]
  , testCase "function key 3"                     $ f mempty "\ESCOR"      [KeyEvent (FunctionKey 3) mempty]
  , testCase "function key 4"                     $ f mempty "\ESCOS"      [KeyEvent (FunctionKey 4) mempty]
  , testCase "function key 5"                     $ f mempty "\ESC[15~"    [KeyEvent (FunctionKey 5) mempty]
  , testCase "function key 6"                     $ f mempty "\ESC[17~"    [KeyEvent (FunctionKey 6) mempty]
  , testCase "function key 7"                     $ f mempty "\ESC[18~"    [KeyEvent (FunctionKey 7) mempty]
  , testCase "function key 8"                     $ f mempty "\ESC[19~"    [KeyEvent (FunctionKey 8) mempty]
  , testCase "function key 9"                     $ f mempty "\ESC[20~"    [KeyEvent (FunctionKey 9) mempty]
  , testCase "function key 10"                    $ f mempty "\ESC[21~"    [KeyEvent (FunctionKey 10) mempty]
  , testCase "function key 11"                    $ f mempty "\ESC[23~"    [KeyEvent (FunctionKey 11) mempty]
  , testCase "function key 12"                    $ f mempty "\ESC[24~"    [KeyEvent (FunctionKey 12) mempty]
  , testCase "function key 12, shift"             $ f mempty "\ESC[24;2~"  [KeyEvent (FunctionKey 12) shiftKey]
  , testCase "function key 12, alt"               $ f mempty "\ESC[24;3~"  [KeyEvent (FunctionKey 12) altKey]
  , testCase "function key 12, shift+alt"         $ f mempty "\ESC[24;4~"  [KeyEvent (FunctionKey 12) (shiftKey <> altKey)]
  , testCase "function key 12, ctrl"              $ f mempty "\ESC[24;5~"  [KeyEvent (FunctionKey 12) ctrlKey]
  , testCase "function key 12, shift+ctrl"        $ f mempty "\ESC[24;6~"  [KeyEvent (FunctionKey 12) (shiftKey <> ctrlKey)]
  , testCase "function key 12, alt+ctrl"          $ f mempty "\ESC[24;7~"  [KeyEvent (FunctionKey 12) (altKey <> ctrlKey)]
  , testCase "function key 12, shift+alt+ctrl"    $ f mempty "\ESC[24;8~"  [KeyEvent (FunctionKey 12) (shiftKey <> altKey <> ctrlKey)]
  ]
  where
    -- The special chars are assumed to be constant on Windows.
    f = assertDecoding sk
    sk mods = \case
        '\r'   -> Just $ KeyEvent EnterKey     mods
        '\n'   -> Just $ KeyEvent EnterKey     mods
        '\t'   -> Just $ KeyEvent TabKey       mods
        '\b'   -> Just $ KeyEvent BackspaceKey mods
        '\SP'  -> Just $ KeyEvent SpaceKey     mods
        '\DEL' -> Just $ KeyEvent BackspaceKey mods
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

assertDecoding :: (Modifiers -> Char -> Maybe Event) -> Modifiers -> String -> [Event] -> Assertion
assertDecoding specialChars mods input expected
  = expected @=? decode (defaultDecoder specialChars) input
  where
    decode :: Decoder -> String -> [Event]
    decode decoder = \case
        [] -> []
        (x:xs) ->  case feedDecoder decoder mods x of
            Left decoder' -> decode decoder' xs
            Right evs     -> evs
