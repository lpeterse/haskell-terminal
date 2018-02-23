{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module System.Terminal.Ansi.Internal where

import           Control.Concurrent
import qualified Control.Concurrent.Async      as A
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception             as E
import           Control.Monad                 (forever, when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bits
import qualified Data.ByteString               as BS
import           Data.Char
import           Data.Function                 (fix)
import           Data.Maybe
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Word
import           System.Environment
import qualified System.IO                     as IO

import qualified Control.Monad.Terminal.Events as T
import qualified Control.Monad.Terminal.Modes  as T

data TerminalEnv
  = TerminalEnv
  { envTermType     :: BS.ByteString
  , envInputChars   :: STM Char
  , envInputEvents  :: STM T.Event
  , envOutput       :: Text.Text -> STM ()
  , envOutputFlush  :: STM ()
  , envSpecialChars :: Char -> Maybe T.Event
  }

class Monad m => MonadInput m where
  getNext         :: m Char
  getNextNonBlock :: m (Maybe Char)

instance MonadInput (ReaderT (STM Char) STM) where
  getNext = ask >>= lift
  getNextNonBlock = ask >>= \mc-> lift ((Just <$> mc) `orElse` pure Nothing)

decodeAnsi :: MonadInput m => m T.Event
decodeAnsi = decode1 =<< getNext
  where
    decode1 :: MonadInput m => Char -> m T.Event
    decode1 c
      -- The first 31 values are control codes.
      -- The ESC character _might_ introduce an escape sequence
      -- and has to be treated specifically.
      -- All other characters are directly mapped to a KChar event.
      | c == '\ESC' = decodeEscape
      | otherwise   = pure $ T.EvKey (T.KChar c) []

    decodeEscape :: MonadInput m => m T.Event
    decodeEscape = getNext >>= \case
      '\NUL' -> pure $ T.EvKey (T.KChar '\ESC') [] -- a single escape is always followed by a filling NUL character (instead of timing)
      x      -> decodeEscapeSequence x

    decodeEscapeSequence :: (MonadInput m) => Char -> m T.Event
    decodeEscapeSequence x
      | x >= '\SOH' && x <= '\EM' = pure $ T.EvKey (T.KChar $ toEnum $ 64 + fromEnum x) [T.MCtrl, T.MAlt] -- urxvt
      | x == '\ESC' = getNext >>= \case
          '\NUL' -> pure $ T.EvUnknownSequence "FNORD"
          '0' -> getNext >>= \case -- seems to just add MAlt to all sequences
            'a' -> pure $ T.EvKey (T.KUp    1) [T.MCtrl, T.MAlt] -- urxvt
            'b' -> pure $ T.EvKey (T.KDown  1) [T.MCtrl, T.MAlt] -- urxvt
            'c' -> pure $ T.EvKey (T.KRight 1) [T.MCtrl, T.MAlt] -- urxvt
            'd' -> pure $ T.EvKey (T.KLeft  1) [T.MCtrl, T.MAlt] -- urxvt
            y   -> pure $ T.EvUnknownSequence ['\ESC', '\ESC', '0', y]
          '[' -> getNext >>= \y-> decodeCSI y >>= \case
            T.EvKey k ms -> pure $ T.EvKey k (T.MAlt:ms) -- urxvt
            ev           -> pure ev
      | x == '\US' = pure $ T.EvKey (T.KChar '-') [T.MAlt, T.MShift] -- urxvt
      | x == ' '   = pure $ T.EvKey (T.KChar '1') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == '!'   = pure $ T.EvKey (T.KChar '3') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == '\\'  = pure $ T.EvKey (T.KChar '4') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == '"'   = pure $ T.EvKey (T.KChar '5') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == '#'   = pure $ T.EvKey (T.KChar '7') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == '$'   = pure $ T.EvKey (T.KChar '\'') [T.MAlt] -- urxvt, gnome-terminal
      | x == '%'   = pure $ T.EvKey (T.KChar '9') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == '&'   = pure $ T.EvKey (T.KChar '0') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == '\''  = pure $ T.EvKey (T.KChar '8') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == '+'   = pure $ T.EvKey (T.KChar '=') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == ','   = pure $ T.EvKey (T.KChar ',') [T.MAlt] -- urxvt, gnome-terminal
      | x == '-'   = pure $ T.EvKey (T.KChar '-') [T.MAlt]
      | x == '.'   = pure $ T.EvKey (T.KChar '.') [T.MAlt] -- urxvt, gnome-terminal
      | x == '/'   = pure $ T.EvKey (T.KChar '/') [T.MAlt] -- urxvt, gnome-terminal
      | x == '0'   = pure $ T.EvKey (T.KChar '0') [T.MAlt]
      | x == '1'   = pure $ T.EvKey (T.KChar '1') [T.MAlt]
      | x == '2'   = pure $ T.EvKey (T.KChar '2') [T.MAlt]
      | x == '3'   = pure $ T.EvKey (T.KChar '3') [T.MAlt]
      | x == '4'   = pure $ T.EvKey (T.KChar '4') [T.MAlt]
      | x == '5'   = pure $ T.EvKey (T.KChar '5') [T.MAlt]
      | x == '6'   = pure $ T.EvKey (T.KChar '6') [T.MAlt]
      | x == '7'   = pure $ T.EvKey (T.KChar '7') [T.MAlt]
      | x == '8'   = pure $ T.EvKey (T.KChar '8') [T.MAlt]
      | x == '9'   = pure $ T.EvKey (T.KChar '9') [T.MAlt]
      | x == ';'   = pure $ T.EvKey (T.KChar ';') [T.MAlt]
      | x == '<'   = pure $ T.EvKey (T.KChar '<') [T.MAlt]
      | x == '='   = pure $ T.EvKey (T.KChar '=') [T.MAlt]
      | x == '>'   = pure $ T.EvKey (T.KChar '>') [T.MAlt, T.MShift]
      | x == '?'   = pure $ T.EvKey (T.KChar '2') [T.MAlt, T.MShift]
      | x == '@'   = pure $ T.EvKey (T.KChar 'A') [T.MAlt, T.MShift]
      | x == 'O'   = getNext >>= \case
                        'P' -> pure $ T.EvKey (T.KFun   1) [] -- gnome-terminal (?)
                        'Q' -> pure $ T.EvKey (T.KFun   2) [] -- gnome-terminal
                        'R' -> pure $ T.EvKey (T.KFun   3) [] -- gnome-terminal
                        'S' -> pure $ T.EvKey (T.KFun   4) [] -- gnome-terminal
                        'a' -> pure $ T.EvKey (T.KUp    1) [T.MCtrl] -- urxvt
                        'b' -> pure $ T.EvKey (T.KDown  1) [T.MCtrl] -- urxvt
                        'c' -> pure $ T.EvKey (T.KRight 1) [T.MCtrl] -- urxvt
                        'd' -> pure $ T.EvKey (T.KLeft  1) [T.MCtrl] -- urxvt
                        xs  -> error (show xs)
      | x == '['   = getNext >>= \case
                        '\NUL' -> pure $ T.EvKey (T.KChar '[') [T.MAlt] -- urxvt, gnome-terminal
                        y      -> decodeCSI y
      | x == '\\'   = pure $ T.EvKey (T.KChar '\\') [T.MAlt] -- urxvt, gnome-terminal
      | x == ']'    = pure $ T.EvKey (T.KChar ']')  [T.MAlt] -- urxvt, gnome-terminal
      | x == '^'    = pure $ T.EvKey (T.KChar '6')  [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == '_'    = pure $ T.EvKey (T.KChar '_')  [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == '`'    = pure $ T.EvKey (T.KChar '`')  [T.MAlt] -- urxvt, gnome-terminal
      | x == '~'    = pure $ T.EvKey (T.KChar '`')  [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == '\DEL' = pure $ T.EvKey T.KDelete []
      | x >= 'a' && x <= 'z' = pure $ T.EvKey (T.KChar x) [T.MAlt]
      | otherwise   = error $ show x

    decodeCSI :: (MonadInput m) => Char -> m T.Event
    decodeCSI y = withParams1 y $ \ps-> \case
      '\ESC'     -> pure $ T.EvKey (T.KChar '[') [T.MAlt]             -- urxvt
      '$'        -> pure $ T.EvKey T.KDelete [T.MAlt, T.MShift]       -- urxvt, gnome-terminal
      '@'        -> undefined -- withN 1 ps $ \n-> pure $ T.EvKey (T.KSpace  n) [] -- in urxvt shift+ctrl+pageup/down causes n==5/6
      'A'        -> decodeArrowKey ps T.KUp
      'B'        -> decodeArrowKey ps T.KDown
      'C'        -> decodeArrowKey ps T.KRight
      'D'        -> decodeArrowKey ps T.KLeft
      'E'        -> undefined
      'F'        -> pure $ T.EvKey T.KEnd []
      'G'        -> undefined
      'H'        -> pure $ T.EvKey T.KHome []
      'I'        -> withN 1 ps $ \n-> pure $ T.EvKey (T.KTab    n) []
      'J'        -> undefined
      'K'        -> undefined
      'L'        -> undefined
      'M'        -> undefined
      'N'        -> undefined
      'O'        -> undefined
      'P'        -> undefined
      'Q'        -> undefined
      'R'        -> withNM 0 0 ps $ \n m-> pure $ T.EvCursorPosition (n,m)
      'S'        -> undefined
      'T'        -> undefined
      'U'        -> undefined
      'V'        -> undefined
      'W'        -> undefined
      'X'        -> undefined
      'Y'        -> undefined
      'Z'        -> withN 1 ps $ \n-> pure $ T.EvKey (T.KBacktab n) []
      '^'        -> case ps of
                      [    '2'] -> pure $ T.EvKey T.KInsert   [T.MCtrl]
                      [    '3'] -> pure $ T.EvKey T.KDelete   [T.MCtrl]
                      [    '4'] -> pure $ T.EvKey T.KPageUp   [T.MCtrl]
                      [    '7'] -> pure $ T.EvKey T.KPageDown [T.MCtrl]
                      [    '5'] -> pure $ T.EvKey T.KHome     [T.MCtrl]
                      [    '6'] -> pure $ T.EvKey T.KEnd      [T.MCtrl]
                      ['1','1'] -> pure $ T.EvKey (T.KFun  1) [T.MCtrl]
                      ['1','2'] -> pure $ T.EvKey (T.KFun  2) [T.MCtrl]
                      ['1','3'] -> pure $ T.EvKey (T.KFun  3) [T.MCtrl]
                      ['1','4'] -> pure $ T.EvKey (T.KFun  4) [T.MCtrl]
                      ['1','5'] -> pure $ T.EvKey (T.KFun  5) [T.MCtrl]
                      ['1','7'] -> pure $ T.EvKey (T.KFun  6) [T.MCtrl]
                      ['1','8'] -> pure $ T.EvKey (T.KFun  7) [T.MCtrl]
                      ['1','9'] -> pure $ T.EvKey (T.KFun  8) [T.MCtrl]
                      ['2','0'] -> pure $ T.EvKey (T.KFun  9) [T.MCtrl]
                      ['2','1'] -> pure $ T.EvKey (T.KFun 10) [T.MCtrl]
                      ['2','2'] -> pure $ T.EvKey (T.KFun 11) [T.MCtrl]
                      ['2','3'] -> pure $ T.EvKey (T.KFun 12) [T.MCtrl]
                      ['2','4'] -> pure $ T.EvKey (T.KFun 13) [T.MCtrl]
                      ['2','5'] -> pure $ T.EvKey (T.KFun 14) [T.MCtrl]
                      ['2','7'] -> pure $ T.EvKey (T.KFun 15) [T.MCtrl]
                      ['2','8'] -> pure $ T.EvKey (T.KFun 16) [T.MCtrl]
                      ['3','1'] -> pure $ T.EvKey (T.KFun 17) [T.MCtrl]
                      ['3','2'] -> pure $ T.EvKey (T.KFun 18) [T.MCtrl]
                      ['3','4'] -> pure $ T.EvKey (T.KFun 19) [T.MCtrl]
                      ['3','5'] -> pure $ T.EvKey (T.KFun 20) [T.MCtrl]
                      _         -> error ("FOOB" ++ show ps)
      'f' -> undefined
      'i' -> pure $ T.EvKey T.KPrtScr []
      'm' -> undefined -- SGR
      '~' -> case ps of
        "2"    -> pure $ T.EvKey T.KInsert []
        "3"    -> pure $ T.EvKey T.KDelete []
        "5"    -> pure $ T.EvKey T.KPageUp []
        "6"    -> pure $ T.EvKey T.KPageDown []
        "9"    -> pure $ T.EvKey T.KHome []
        "10"   -> pure $ T.EvKey T.KEnd []
        "11"   -> pure $ T.EvKey (T.KFun 1) []
        "12"   -> pure $ T.EvKey (T.KFun 2) []
        "13"   -> pure $ T.EvKey (T.KFun 3) []
        "14"   -> pure $ T.EvKey (T.KFun 4) []
        "15"   -> pure $ T.EvKey (T.KFun 5) []
        "17"   -> pure $ T.EvKey (T.KFun 6) []
        "18"   -> pure $ T.EvKey (T.KFun 7) []
        "19"   -> pure $ T.EvKey (T.KFun 8) []
        "20"   -> pure $ T.EvKey (T.KFun 9) []
        "21"   -> pure $ T.EvKey (T.KFun 10) []
        "22"   -> pure $ T.EvKey (T.KFun 11) []
        "23"   -> pure $ T.EvKey (T.KFun 12) []
        "24"   -> pure $ T.EvKey (T.KFun 13) []
        "25"   -> pure $ T.EvKey (T.KFun 14) []
        "27"   -> pure $ T.EvKey (T.KFun 15) []
        "28"   -> pure $ T.EvKey (T.KFun 16) []
        "31"   -> pure $ T.EvKey (T.KFun 17) []
        "32"   -> pure $ T.EvKey (T.KFun 18) []
        "33"   -> pure $ T.EvKey (T.KFun 19) []
        "34"   -> pure $ T.EvKey (T.KFun 20) []
        "15;5" -> pure $ T.EvKey (T.KFun  5) [T.MCtrl]         -- gnome-terminal
        "17;5" -> pure $ T.EvKey (T.KFun  6) [T.MCtrl]         -- gnome-terminal
        "18;5" -> pure $ T.EvKey (T.KFun  7) [T.MCtrl]         -- gnome-terminal
        "19;5" -> pure $ T.EvKey (T.KFun  8) [T.MCtrl]         -- gnome-terminal
        "20;5" -> pure $ T.EvKey (T.KFun  9) [T.MCtrl]         -- gnome-terminal
        "21;5" -> pure $ T.EvKey (T.KFun 10) [T.MCtrl]         -- gnome-terminal
        "22;5" -> pure $ T.EvKey (T.KFun 11) [T.MCtrl]         -- gnome-terminal
        "23;5" -> pure $ T.EvKey (T.KFun 12) [T.MCtrl]         -- gnome-terminal
        "2;3"  -> pure $ T.EvKey T.KInsert [T.MAlt]            -- gnome-terminal
        "3;5"  -> pure $ T.EvKey T.KDelete [T.MCtrl]           -- xterm
        "3;3"  -> pure $ T.EvKey T.KDelete [T.MAlt]            -- gnome-terminal
        "5;4"  -> pure $ T.EvKey T.KPageUp [T.MCtrl]           -- gnome-terminal
        "5;5"  -> pure $ T.EvKey T.KPageDown [T.MCtrl]         -- gnome-terminal
        "5;3"  -> pure $ T.EvKey T.KPageUp [T.MAlt]            -- gnome-terminal
        "6;3"  -> pure $ T.EvKey T.KPageDown [T.MAlt]          -- gnome-terminal
        "5;7"  -> pure $ T.EvKey T.KPageUp [T.MCtrl, T.MAlt]   -- gnome-terminal
        "6;7"  -> pure $ T.EvKey T.KPageDown [T.MCtrl, T.MAlt] -- gnome-terminal
        _      {- .. -} -> error $ show ps
      x   -> error $ "HERE" ++ show x
      where
        decodeArrowKey ps key = withNumbers ps $ \case
          []    -> pure $ T.EvKey (key 1) []
          [n]   -> pure $ T.EvKey (key $ if n == 0 then 1 else n) []
          [1,3] -> pure $ T.EvKey (key 1) [T.MAlt]          -- gnome-terminal
          [1,5] -> pure $ T.EvKey (key 1) [T.MCtrl]         -- gnome-terminal
          [1,7] -> pure $ T.EvKey (key 1) [T.MCtrl, T.MAlt] -- gnome-terminal
          xs    -> error (show xs)

withParams1 :: MonadInput m => Char -> ([Char] -> Char -> m a) -> m a
withParams1 x f
  | x >= '0' && x <= '?' = withParameters 256 [x]
  | otherwise            = f [] x
  where
    withParameters 0 _            = fail "CSI: LENGTH LIMIT EXCEEDED"
    withParameters limit ps       = getNext >>= \case
      y | y >= '0' && y <= '9' -> withParameters (limit - 1) $! y:ps
        | otherwise            -> f (reverse ps) y

withN :: Monad m => Int -> [Char] -> (Int -> m a) -> m a
withN defN [] f               = f defN
withN _    ps f               = g ps 0
  where
    g [] i                    = f i
    g (x:xs) i
      | x >= '0' && x <= '9'  = g xs $! i * 10 - 48 + fromEnum x
      | otherwise             = error $ "CSI: INVALID NUMBER " ++ show x

withNM :: Monad m => Int -> Int -> [Char] -> (Int -> Int -> m a) -> m a
withNM defN defM []       f  = f defN defM
withNM defN defM [';']    f  = f defN defM
withNM defN defM (';':ps) f  = withN defM ps (f defN)
withNM defN defM      ps  f  = g ps 0
  where
    g [] i                   = fail "CSI: INVALID NUMBER"
    g (x:xs) i
      | x == ';'             = withN defM xs (f i)
      | x >= '0' && x <= '9' = g xs $! i * 10 - 48 + fromEnum x
      | otherwise            = fail "CSI: INVALID NUMBER"

withNumbers :: (Monad m) => [Char] -> ([Int] -> m a) -> m a
withNumbers xs f = numbers' xs 0 >>= f
  where
    numbers' [] i
      = pure [i]
    numbers' (x:xs) i
      | x >= '0' && x <= '9' = numbers' xs (i * 10 - 48 + fromEnum x)
      | x == ';'             = (i:) <$> numbers' xs 0
      | otherwise            = fail ""
