{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Control.Monad.Terminal.Ansi.Decoder where

import           Control.Monad                (forever, when)
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Char
import           Data.Maybe
import           Data.Word

import qualified Control.Monad.Terminal.Input as T

class Monad m => MonadAnsiInput m where
  getNext         :: m Char
  getNextNonBlock :: m (Maybe Char)

instance MonadAnsiInput (ReaderT (STM Char) STM) where
  getNext = ask >>= lift
  getNextNonBlock = ask >>= \mc-> lift ((Just <$> mc) `orElse` pure Nothing)

decodeAnsi :: MonadAnsiInput m => m T.Event
decodeAnsi = decode1 =<< getNext
  where
    decode1 :: MonadAnsiInput m => Char -> m T.Event
    decode1 c
      -- The first 31 values are control codes.
      -- The ESC character _might_ introduce an escape sequence
      -- and has to be treated specifically.
      -- All other characters are directly mapped to a CharKey event.
      | c == '\ESC'               = decodeEscape
      | c >= '\SOH' && c <= '\US' = pure $ T.KeyEvent (T.CharKey (toEnum $ (+64) $ fromEnum c)) T.ctrlKey
      | otherwise                 = pure $ T.KeyEvent (T.CharKey c) mempty

    decodeEscape :: MonadAnsiInput m => m T.Event
    decodeEscape = getNext >>= \case
      '\NUL' -> pure $ T.KeyEvent (T.CharKey '\ESC') mempty -- a single escape is always followed by a filling NUL character (instead of timing)
      x      -> decodeEscapeSequence x

    decodeEscapeSequence :: (MonadAnsiInput m) => Char -> m T.Event
    decodeEscapeSequence x
      | x >= '\SOH' && x <= '\EM' = pure $ T.KeyEvent (T.CharKey $ toEnum $ 64 + fromEnum x) (T.ctrlKey `mappend` T.altKey) -- urxvt
      | x == '\ESC' = getNext >>= \case
          '\NUL' -> pure $ T.OtherEvent "FNORD"
          '0' -> getNext >>= \case -- seems to just add MAlt to all sequences
            'a' -> pure $ T.KeyEvent (T.ArrowKey T.Upwards)    (T.ctrlKey `mappend` T.altKey) -- urxvt
            'b' -> pure $ T.KeyEvent (T.ArrowKey T.Downwards)  (T.ctrlKey `mappend` T.altKey) -- urxvt
            'c' -> pure $ T.KeyEvent (T.ArrowKey T.Rightwards) (T.ctrlKey `mappend` T.altKey) -- urxvt
            'd' -> pure $ T.KeyEvent (T.ArrowKey T.Leftwards)  (T.ctrlKey `mappend` T.altKey) -- urxvt
            y   -> pure $ T.OtherEvent ['\ESC', '\ESC', '0', y]
          '[' -> getNext >>= \y-> decodeCSI y >>= \case
            T.KeyEvent k ms -> pure $ T.KeyEvent k (T.altKey `mappend` ms) -- urxvt
            ev           -> pure ev
      | x == '\US' = pure $ T.KeyEvent (T.CharKey '-') (T.altKey `mappend` T.shiftKey) -- urxvt
      | x == ' '   = pure $ T.KeyEvent (T.CharKey '1') (T.altKey `mappend` T.shiftKey) -- urxvt, gnome-terminal
      | x == '!'   = pure $ T.KeyEvent (T.CharKey '3') (T.altKey `mappend` T.shiftKey) -- urxvt, gnome-terminal
      | x == '\\'  = pure $ T.KeyEvent (T.CharKey '4') (T.altKey `mappend` T.shiftKey) -- urxvt, gnome-terminal
      | x == '"'   = pure $ T.KeyEvent (T.CharKey '5') (T.altKey `mappend` T.shiftKey) -- urxvt, gnome-terminal
      | x == '#'   = pure $ T.KeyEvent (T.CharKey '7') (T.altKey `mappend` T.shiftKey) -- urxvt, gnome-terminal
      | x == '$'   = pure $ T.KeyEvent (T.CharKey '\'') T.altKey -- urxvt, gnome-terminal
      | x == '%'   = pure $ T.KeyEvent (T.CharKey '9') (T.altKey `mappend` T.shiftKey) -- urxvt, gnome-terminal
      | x == '&'   = pure $ T.KeyEvent (T.CharKey '0') (T.altKey `mappend` T.shiftKey) -- urxvt, gnome-terminal
      | x == '\''  = pure $ T.KeyEvent (T.CharKey '8') (T.altKey `mappend` T.shiftKey) -- urxvt, gnome-terminal
      | x == '+'   = pure $ T.KeyEvent (T.CharKey '=') (T.altKey `mappend` T.shiftKey) -- urxvt, gnome-terminal
      | x == ','   = pure $ T.KeyEvent (T.CharKey ',') T.altKey -- urxvt, gnome-terminal
      | x == '-'   = pure $ T.KeyEvent (T.CharKey '-') T.altKey
      | x == '.'   = pure $ T.KeyEvent (T.CharKey '.') T.altKey -- urxvt, gnome-terminal
      | x == '/'   = pure $ T.KeyEvent (T.CharKey '/') T.altKey -- urxvt, gnome-terminal
      | x == '0'   = pure $ T.KeyEvent (T.CharKey '0') T.altKey
      | x == '1'   = pure $ T.KeyEvent (T.CharKey '1') T.altKey
      | x == '2'   = pure $ T.KeyEvent (T.CharKey '2') T.altKey
      | x == '3'   = pure $ T.KeyEvent (T.CharKey '3') T.altKey
      | x == '4'   = pure $ T.KeyEvent (T.CharKey '4') T.altKey
      | x == '5'   = pure $ T.KeyEvent (T.CharKey '5') T.altKey
      | x == '6'   = pure $ T.KeyEvent (T.CharKey '6') T.altKey
      | x == '7'   = pure $ T.KeyEvent (T.CharKey '7') T.altKey
      | x == '8'   = pure $ T.KeyEvent (T.CharKey '8') T.altKey
      | x == '9'   = pure $ T.KeyEvent (T.CharKey '9') T.altKey
      | x == ';'   = pure $ T.KeyEvent (T.CharKey ';') T.altKey
      | x == '<'   = pure $ T.KeyEvent (T.CharKey '<') T.altKey
      | x == '='   = pure $ T.KeyEvent (T.CharKey '=') T.altKey
      | x == '>'   = pure $ T.KeyEvent (T.CharKey '>') (T.altKey `mappend` T.shiftKey)
      | x == '?'   = pure $ T.KeyEvent (T.CharKey '2') (T.altKey `mappend` T.shiftKey)
      | x == '@'   = pure $ T.KeyEvent (T.CharKey 'A') (T.altKey `mappend` T.shiftKey)
      | x == 'O'   = getNext >>= \case
                        'P' -> pure $ T.KeyEvent (T.FunctionKey   1) mempty -- gnome-terminal (?)
                        'Q' -> pure $ T.KeyEvent (T.FunctionKey   2) mempty -- gnome-terminal
                        'R' -> pure $ T.KeyEvent (T.FunctionKey   3) mempty -- gnome-terminal
                        'S' -> pure $ T.KeyEvent (T.FunctionKey   4) mempty -- gnome-terminal
                        'a' -> pure $ T.KeyEvent (T.ArrowKey T.Upwards)    T.ctrlKey -- urxvt
                        'b' -> pure $ T.KeyEvent (T.ArrowKey T.Downwards)  T.ctrlKey -- urxvt
                        'c' -> pure $ T.KeyEvent (T.ArrowKey T.Rightwards) T.ctrlKey -- urxvt
                        'd' -> pure $ T.KeyEvent (T.ArrowKey T.Leftwards)  T.ctrlKey -- urxvt
                        xs  -> error (show xs)
      | x == '['   = getNext >>= \case
                        '\NUL' -> pure $ T.KeyEvent (T.CharKey '[') T.altKey -- urxvt, gnome-terminal
                        y      -> decodeCSI y
      | x == '\\'   = pure $ T.KeyEvent (T.CharKey '\\') T.altKey -- urxvt, gnome-terminal
      | x == ']'    = pure $ T.KeyEvent (T.CharKey ']')  T.altKey -- urxvt, gnome-terminal
      | x == '^'    = pure $ T.KeyEvent (T.CharKey '6')  (T.altKey `mappend` T.shiftKey) -- urxvt, gnome-terminal
      | x == '_'    = pure $ T.KeyEvent (T.CharKey '_')  (T.altKey `mappend` T.shiftKey) -- urxvt, gnome-terminal
      | x == '`'    = pure $ T.KeyEvent (T.CharKey '`')  T.altKey -- urxvt, gnome-terminal
      | x == '~'    = pure $ T.KeyEvent (T.CharKey '`')  (T.altKey `mappend` T.shiftKey) -- urxvt, gnome-terminal
      | x == '\DEL' = pure $ T.KeyEvent T.DeleteKey mempty
      | x >= 'a' && x <= 'z' = pure $ T.KeyEvent (T.CharKey x) T.altKey
      | otherwise   = error $ show x

    decodeCSI :: (MonadAnsiInput m) => Char -> m T.Event
    decodeCSI y = withParams1 y $ \ps-> \case
      '\ESC'     -> pure $ T.KeyEvent (T.CharKey '[') T.altKey             -- urxvt
      '$'        -> pure $ T.KeyEvent T.DeleteKey (T.altKey `mappend` T.shiftKey)       -- urxvt, gnome-terminal
      '@'        -> undefined -- withN 1 ps $ \n-> pure $ T.KeyEvent (T.KSpace  n) mempty -- in urxvt shift+ctrl+pageup/down causes n==5/6
      'A'        -> decodeArrowKey ps (T.ArrowKey T.Upwards)
      'B'        -> decodeArrowKey ps (T.ArrowKey T.Downwards)
      'C'        -> decodeArrowKey ps (T.ArrowKey T.Rightwards)
      'D'        -> decodeArrowKey ps (T.ArrowKey T.Leftwards)
      'E'        -> undefined
      'F'        -> pure $ T.KeyEvent T.EndKey mempty
      'G'        -> undefined
      'H'        -> pure $ T.KeyEvent T.HomeKey mempty
      'I'        -> withN 1 ps $ \n-> pure $ T.KeyEvent T.TabKey mempty
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
      'Z'        -> withN 1 ps $ \n-> pure $ T.KeyEvent T.TabKey T.shiftKey
      '^'        -> case ps of
                      [    '2'] -> pure $ T.KeyEvent T.InsertKey   T.ctrlKey
                      [    '3'] -> pure $ T.KeyEvent T.DeleteKey T.ctrlKey
                      [    '4'] -> pure $ T.KeyEvent T.PageUpKey   T.ctrlKey
                      [    '7'] -> pure $ T.KeyEvent T.PageDownKey T.ctrlKey
                      [    '5'] -> pure $ T.KeyEvent T.HomeKey     T.ctrlKey
                      [    '6'] -> pure $ T.KeyEvent T.EndKey      T.ctrlKey
                      ['1','1'] -> pure $ T.KeyEvent (T.FunctionKey  1) T.ctrlKey
                      ['1','2'] -> pure $ T.KeyEvent (T.FunctionKey  2) T.ctrlKey
                      ['1','3'] -> pure $ T.KeyEvent (T.FunctionKey  3) T.ctrlKey
                      ['1','4'] -> pure $ T.KeyEvent (T.FunctionKey  4) T.ctrlKey
                      ['1','5'] -> pure $ T.KeyEvent (T.FunctionKey  5) T.ctrlKey
                      ['1','7'] -> pure $ T.KeyEvent (T.FunctionKey  6) T.ctrlKey
                      ['1','8'] -> pure $ T.KeyEvent (T.FunctionKey  7) T.ctrlKey
                      ['1','9'] -> pure $ T.KeyEvent (T.FunctionKey  8) T.ctrlKey
                      ['2','0'] -> pure $ T.KeyEvent (T.FunctionKey  9) T.ctrlKey
                      ['2','1'] -> pure $ T.KeyEvent (T.FunctionKey 10) T.ctrlKey
                      ['2','2'] -> pure $ T.KeyEvent (T.FunctionKey 11) T.ctrlKey
                      ['2','3'] -> pure $ T.KeyEvent (T.FunctionKey 12) T.ctrlKey
                      ['2','4'] -> pure $ T.KeyEvent (T.FunctionKey 13) T.ctrlKey
                      ['2','5'] -> pure $ T.KeyEvent (T.FunctionKey 14) T.ctrlKey
                      ['2','7'] -> pure $ T.KeyEvent (T.FunctionKey 15) T.ctrlKey
                      ['2','8'] -> pure $ T.KeyEvent (T.FunctionKey 16) T.ctrlKey
                      ['3','1'] -> pure $ T.KeyEvent (T.FunctionKey 17) T.ctrlKey
                      ['3','2'] -> pure $ T.KeyEvent (T.FunctionKey 18) T.ctrlKey
                      ['3','4'] -> pure $ T.KeyEvent (T.FunctionKey 19) T.ctrlKey
                      ['3','5'] -> pure $ T.KeyEvent (T.FunctionKey 20) T.ctrlKey
                      _         -> error ("FOOB" ++ show ps)
      'f' -> undefined
      'i' -> pure $ T.KeyEvent T.PrintKey mempty
      'm' -> undefined -- SGR
      '~' -> case ps of
        "2"    -> pure $ T.KeyEvent T.InsertKey mempty
        "3"    -> pure $ T.KeyEvent T.DeleteKey mempty
        "5"    -> pure $ T.KeyEvent T.PageUpKey mempty
        "6"    -> pure $ T.KeyEvent T.PageDownKey mempty
        "9"    -> pure $ T.KeyEvent T.HomeKey mempty
        "10"   -> pure $ T.KeyEvent T.EndKey mempty
        "11"   -> pure $ T.KeyEvent (T.FunctionKey 1) mempty
        "12"   -> pure $ T.KeyEvent (T.FunctionKey 2) mempty
        "13"   -> pure $ T.KeyEvent (T.FunctionKey 3) mempty
        "14"   -> pure $ T.KeyEvent (T.FunctionKey 4) mempty
        "15"   -> pure $ T.KeyEvent (T.FunctionKey 5) mempty
        "17"   -> pure $ T.KeyEvent (T.FunctionKey 6) mempty
        "18"   -> pure $ T.KeyEvent (T.FunctionKey 7) mempty
        "19"   -> pure $ T.KeyEvent (T.FunctionKey 8) mempty
        "20"   -> pure $ T.KeyEvent (T.FunctionKey 9) mempty
        "21"   -> pure $ T.KeyEvent (T.FunctionKey 10) mempty
        "22"   -> pure $ T.KeyEvent (T.FunctionKey 11) mempty
        "23"   -> pure $ T.KeyEvent (T.FunctionKey 12) mempty
        "24"   -> pure $ T.KeyEvent (T.FunctionKey 13) mempty
        "25"   -> pure $ T.KeyEvent (T.FunctionKey 14) mempty
        "27"   -> pure $ T.KeyEvent (T.FunctionKey 15) mempty
        "28"   -> pure $ T.KeyEvent (T.FunctionKey 16) mempty
        "31"   -> pure $ T.KeyEvent (T.FunctionKey 17) mempty
        "32"   -> pure $ T.KeyEvent (T.FunctionKey 18) mempty
        "33"   -> pure $ T.KeyEvent (T.FunctionKey 19) mempty
        "34"   -> pure $ T.KeyEvent (T.FunctionKey 20) mempty
        "15;5" -> pure $ T.KeyEvent (T.FunctionKey  5) T.ctrlKey         -- gnome-terminal
        "17;5" -> pure $ T.KeyEvent (T.FunctionKey  6) T.ctrlKey         -- gnome-terminal
        "18;5" -> pure $ T.KeyEvent (T.FunctionKey  7) T.ctrlKey         -- gnome-terminal
        "19;5" -> pure $ T.KeyEvent (T.FunctionKey  8) T.ctrlKey         -- gnome-terminal
        "20;5" -> pure $ T.KeyEvent (T.FunctionKey  9) T.ctrlKey         -- gnome-terminal
        "21;5" -> pure $ T.KeyEvent (T.FunctionKey 10) T.ctrlKey         -- gnome-terminal
        "22;5" -> pure $ T.KeyEvent (T.FunctionKey 11) T.ctrlKey         -- gnome-terminal
        "23;5" -> pure $ T.KeyEvent (T.FunctionKey 12) T.ctrlKey         -- gnome-terminal
        "2;3"  -> pure $ T.KeyEvent T.InsertKey T.altKey            -- gnome-terminal
        "3;5"  -> pure $ T.KeyEvent T.DeleteKey T.ctrlKey           -- xterm
        "3;3"  -> pure $ T.KeyEvent T.DeleteKey T.altKey            -- gnome-terminal
        "5;4"  -> pure $ T.KeyEvent T.PageUpKey T.ctrlKey           -- gnome-terminal
        "5;5"  -> pure $ T.KeyEvent T.PageDownKey T.ctrlKey         -- gnome-terminal
        "5;3"  -> pure $ T.KeyEvent T.PageUpKey T.altKey            -- gnome-terminal
        "6;3"  -> pure $ T.KeyEvent T.PageDownKey T.altKey          -- gnome-terminal
        "5;7"  -> pure $ T.KeyEvent T.PageUpKey (T.ctrlKey `mappend` T.altKey)   -- gnome-terminal
        "6;7"  -> pure $ T.KeyEvent T.PageDownKey (T.ctrlKey `mappend` T.altKey) -- gnome-terminal
        _      -> error $ show ps
      x   -> error $ "HERE" ++ show x
      where
        decodeArrowKey ps key = withNumbers ps $ \case
          []    -> pure $ T.KeyEvent key mempty
          [_]   -> pure $ T.KeyEvent key mempty
          [1,3] -> pure $ T.KeyEvent key T.altKey          -- gnome-terminal
          [1,5] -> pure $ T.KeyEvent key T.ctrlKey         -- gnome-terminal
          [1,7] -> pure $ T.KeyEvent key (T.ctrlKey `mappend` T.altKey) -- gnome-terminal
          xs    -> error (show xs)

withParams1 :: MonadAnsiInput m => Char -> ([Char] -> Char -> m a) -> m a
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
