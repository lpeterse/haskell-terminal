{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Lazy
import qualified Data.ByteString                as BS
import           Data.Function                  (fix)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import           Data.Traversable

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Control.Monad.Terminal.Events  as T
import qualified System.Terminal.Ansi.Internal  as T

main :: IO ()
main = defaultMain $ testGroup "System.Terminal.Ansi"
  [ testAnsiDecoder
  ]

testAnsiDecoder :: TestTree
testAnsiDecoder = testGroup "decode stream of ANSI escape sequences"
  [ testDecoding "Empty input" [] []
  , testDecoding "NUL character" ["\NUL"] [T.EvKey T.KNull []]
  , testGroup "Control characters" $ (<$> ['A'..'_']) $ \c->
      testDecoding ("Ctrl + " ++ [c]) [BS.pack [fromIntegral $ (fromEnum c) - 64]] [T.EvKey (T.KChar c) [T.MCtrl]]
  , testDecoding "ESC character (standalone)" ["\ESC", "a"] [T.EvKey (T.KChar 'a') [T.MAlt]]
  , testDecoding "ESC character (introducing escape sequence)" ["\ESC", "", "a"] [T.EvKey (T.KChar '[') [T.MCtrl], char 'a']
  , testDecoding "DEL character" ["\DEL"] [T.EvKey (T.KChar '\DEL') []]
  , testGroup "Unicode characters"
    [ testDecoding "two byte character" ["\208\128"] [T.EvKey (T.KChar '\1024') []]
    , testDecoding "three byte character" ["\226\152\131"] [T.EvKey (T.KChar '\x2603') []]
    , testDecoding "three byte character (with delay 1)" ["\226","","\152\131"] [T.EvKey (T.KChar '\x2603') []]
    , testDecoding "three byte character (with delay 2)" ["\226\152", "", "\131"] [T.EvKey (T.KChar '\x2603') []]
    , testGroup "Illegal sequences"
      [ testDecoding "sequence [0b10000000]" [BS.pack [0b10000000]] [T.EvKey (T.KChar '�') []]
      , testDecoding "sequence [0b10000101]" [BS.pack [0b10000000]] [T.EvKey (T.KChar '�') []]
      , testDecoding "sequence [0b11111000]" [BS.pack [0b11111000]] [T.EvKey (T.KChar '�') []]
      , testDecoding "sequence [0b11111001]" [BS.pack [0b11111001]] [T.EvKey (T.KChar '�') []]
      , testDecoding "sequence [0b11111010]" [BS.pack [0b11111010]] [T.EvKey (T.KChar '�') []]
      , testDecoding "sequence [0b11111011]" [BS.pack [0b11111011]] [T.EvKey (T.KChar '�') []]
      , testDecoding "sequence [0b11111100]" [BS.pack [0b11111100]] [T.EvKey (T.KChar '�') []]
      , testDecoding "sequence [0b11111101]" [BS.pack [0b11111101]] [T.EvKey (T.KChar '�') []]
      , testDecoding "sequence [0b11111110]" [BS.pack [0b11111110]] [T.EvKey (T.KChar '�') []]
      , testDecoding "sequence [0b11111111]" [BS.pack [0b11111111]] [T.EvKey (T.KChar '�') []]
      , testDecoding "sequence [0b11111111]" [BS.pack [0b11111111]] [T.EvKey (T.KChar '�') []]
      , testDecoding "sequence [226,152,65]" [BS.pack [226,152,65]] [T.EvKey (T.KChar '�') []]
      ]
    ]
  , testGroup "Report cursor position"
    [ testDecoding "[R"      ["\ESC[R"]      [T.EvCursorPosition (0,0)]
    , testDecoding "[;R"     ["\ESC[;R"]     [T.EvCursorPosition (0,0)]
    , testDecoding "[0;R"    ["\ESC[0;R"]    [T.EvCursorPosition (0,0)]
    , testDecoding "[;0R"    ["\ESC[;0R"]    [T.EvCursorPosition (0,0)]
    , testDecoding "[1;2R"   ["\ESC[1;2R"]   [T.EvCursorPosition (1,2)]
    , testDecoding "[23;42R" ["\ESC[23;42R"] [T.EvCursorPosition (23,42)]
    ]
  ]
  where
    char c = T.EvKey (T.KChar c) []

testDecoding :: String -> [BS.ByteString] -> [T.Event] -> TestTree
testDecoding description input output =
  testCase description $
    matchInputOutput input output

newtype DeterministicInputM a = DeterministicInputM (MaybeT (State [BS.ByteString]) a)
  deriving (Functor, Applicative, Monad)

instance T.MonadInput DeterministicInputM where
  getNext = DeterministicInputM $ fix $ \again-> lift get >>= \case
    [] -> fail "EOF"
    (bs:bss) -> case BS.uncons bs of
      Nothing      -> lift (put bss) >> again
      Just (w8,ts) -> lift (put $ ts:bss) >> pure w8
  getNextNonBlock = DeterministicInputM $ lift get >>= \case
    []       -> pure Nothing
    (bs:bss) -> case BS.uncons bs of
      Nothing      -> pure Nothing -- Timing barrier
      Just (w8,ts) -> lift (put $ ts:bss) >> pure (Just w8)
  wait = DeterministicInputM $ lift get >>= \case
    ("":xs) -> lift (put xs)
    _       -> pure ()

matchInputOutput :: [BS.ByteString] -> [T.Event] -> Assertion
matchInputOutput input output = output @=? evs input
  where
    DeterministicInputM ma = T.decodeAnsi
    evs inp                = case runState (runMaybeT ma) inp of
      (Just ev, inp') -> ev:(evs inp')
      (Nothing, _)    -> []
