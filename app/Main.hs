{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Bits
import qualified Data.ByteString           as BS
import           Data.Word
import           System.IO
import qualified System.Terminal           as T
import qualified System.Terminal.Events    as E

data Color
  = ColorDefault
  | Color4Bit  Color3Bit Bool
  | Color8Bit  Word8
  | Color24Bit Word8 Word8 Word8
  deriving (Eq, Ord, Show)

data Color3Bit
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Eq, Ord, Show)

data SGR
  = Reset
  | Bold
  | Underline
  | SlowBlink
  | ReverseVideo
  | SetForegroundColor Color
  | SetBackgroundColor Color
  | OtherSGR Int
  deriving (Eq, Ord, Show)

main :: IO ()
main = withoutEcho $ withRawMode $ runInputT $ forever $ do
  ev <- decodeAnsi
  liftIO $ putStr (show ev ++ ": ")
  liftIO $ when (E.isBackspace ev) (putStr " isBackspace ")
  liftIO $ putStrLn ""

decodeAnsi :: MonadInput m => m E.Event
decodeAnsi = decode1 =<< getNext
  where
    decode1 :: MonadInput m => Word8 -> m E.Event
    decode1 x
      -- The first 31 values are control codes.
      -- They are mapped as lower case letter + MCtrl modifier.
      | x ==  0   = pure $ E.EvKey E.KNull  []
      | x <= 26   = pure $ E.EvKey (E.KChar [] $ toEnum $ 96 + fromIntegral x) [E.MCtrl]
      -- The escape control code might or might not introduce an escape sequence.
      -- `decodeEscape` handles this by analysing the timing.
      | x == 27   = decodeEscape
      -- The next 4 are '\\', ']', '^' and '-'.
      -- They have a different offset as they are above the upper case letters.
      | x == 28   = pure $ E.EvKey (E.KChar [] '\\') [E.MCtrl]
      | x == 29   = pure $ E.EvKey (E.KChar [] ']')  [E.MCtrl]
      | x == 30   = pure $ E.EvKey (E.KChar [] '^')  [E.MCtrl]
      | x == 31   = pure $ E.EvKey (E.KChar [] '_')  [E.MCtrl]
      -- All following values are printable except 127 which is interpreted as backspace (terminal dependant!)
      | x == 127  = pure $ E.EvKey (E.KBackspace 1) []
      | otherwise = flip E.EvKey [] . E.KChar [] <$> decodeUtf8Sequence x

    decodeEscape :: (MonadInput m) => m E.Event
    decodeEscape = getNextNonBlock >>= \case
      Nothing -> do
        wait -- a single escape can only be distinguished by timing
        getNextNonBlock >>= \case
          Nothing -> pure $ E.EvKey (E.KChar [E.MCtrl] '[') []
          Just x  -> decodeEscapeSequence x
      Just x  -> decodeEscapeSequence x

    decodeEscapeSequence :: (MonadInput m) => Word8 -> m E.Event
    decodeEscapeSequence x
      | x >= 1 && x <= 25 = pure $ E.EvKey (E.KChar [] $ toEnum $ 64 + fromIntegral x) [E.MCtrl, E.MAlt] -- urxvt
      | x == 27   = getNext >>= \case
                      79 -> getNext >>= \case -- seems to just add MAlt to all sequences
                        97  -> pure $ E.EvKey (E.KUp    1) [E.MCtrl, E.MAlt] -- urxvt
                        98  -> pure $ E.EvKey (E.KDown  1) [E.MCtrl, E.MAlt] -- urxvt
                        99  -> pure $ E.EvKey (E.KRight 1) [E.MCtrl, E.MAlt] -- urxvt
                        100 -> pure $ E.EvKey (E.KLeft  1) [E.MCtrl, E.MAlt] -- urxvt
                        y   -> unknownSequence [27, 27, 79, y]
                      91 -> getNext >>= \y-> decodeCSI y >>= \case
                        E.EvKey k ms -> pure $ E.EvKey k (E.MAlt:ms) -- urxvt
                        ev           -> pure ev
      | x == 31   = pure $ E.EvKey (E.KChar [] '-') [E.MAlt, E.MShift] -- urxvt
      | x == 33   = pure $ E.EvKey (E.KChar [] '1') [E.MAlt, E.MShift] -- urxvt, gnome-terminal
      | x == 35   = pure $ E.EvKey (E.KChar [] '3') [E.MAlt, E.MShift] -- urxvt, gnome-terminal
      | x == 36   = pure $ E.EvKey (E.KChar [] '4') [E.MAlt, E.MShift] -- urxvt, gnome-terminal
      | x == 37   = pure $ E.EvKey (E.KChar [] '5') [E.MAlt, E.MShift] -- urxvt, gnome-terminal
      | x == 38   = pure $ E.EvKey (E.KChar [] '7') [E.MAlt, E.MShift] -- urxvt, gnome-terminal
      | x == 39   = pure $ E.EvKey (E.KChar [] '\'') [E.MAlt] -- urxvt, gnome-terminal
      | x == 40   = pure $ E.EvKey (E.KChar [] '9') [E.MAlt, E.MShift] -- urxvt, gnome-terminal
      | x == 41   = pure $ E.EvKey (E.KChar [] '0') [E.MAlt, E.MShift] -- urxvt, gnome-terminal
      | x == 42   = pure $ E.EvKey (E.KChar [] '8') [E.MAlt, E.MShift] -- urxvt, gnome-terminal
      | x == 43   = pure $ E.EvKey (E.KChar [] '=') [E.MAlt, E.MShift] -- urxvt, gnome-terminal
      | x == 44   = pure $ E.EvKey (E.KChar [] ',') [E.MAlt] -- urxvt, gnome-terminal
      | x == 45   = pure $ E.EvKey (E.KChar [] '-') [E.MAlt]
      | x == 46   = pure $ E.EvKey (E.KChar [] '.') [E.MAlt] -- urxvt, gnome-terminal
      | x == 47   = pure $ E.EvKey (E.KChar [] '/') [E.MAlt] -- urxvt, gnome-terminal
      | x == 48   = pure $ E.EvKey (E.KChar [] '0') [E.MAlt]
      | x == 49   = pure $ E.EvKey (E.KChar [] '1') [E.MAlt]
      | x == 50   = pure $ E.EvKey (E.KChar [] '2') [E.MAlt]
      | x == 51   = pure $ E.EvKey (E.KChar [] '3') [E.MAlt]
      | x == 52   = pure $ E.EvKey (E.KChar [] '4') [E.MAlt]
      | x == 53   = pure $ E.EvKey (E.KChar [] '5') [E.MAlt]
      | x == 54   = pure $ E.EvKey (E.KChar [] '6') [E.MAlt]
      | x == 55   = pure $ E.EvKey (E.KChar [] '7') [E.MAlt]
      | x == 56   = pure $ E.EvKey (E.KChar [] '8') [E.MAlt]
      | x == 57   = pure $ E.EvKey (E.KChar [] '9') [E.MAlt]
      | x == 59   = pure $ E.EvKey (E.KChar [] ';') [E.MAlt]
      | x == 60   = pure $ E.EvKey (E.KChar [] '<') [E.MAlt]
      | x == 61   = getNextNonBlock >>= \case
                      Nothing -> pure $ E.EvKey (E.KChar [] '=') [E.MAlt]
                      Just y  -> error (show y)
      | x == 62   = pure $ E.EvKey (E.KChar [] '<') [E.MAlt, E.MShift]
      | x == 64   = pure $ E.EvKey (E.KChar [] '2') [E.MAlt, E.MShift]
      | x == 65   = pure $ E.EvKey (E.KChar [] 'A') [E.MAlt, E.MShift]
      | x == 79   = getNext >>= \case
                      80  -> pure $ E.EvKey (E.KFun   1) [] -- gnome-terminal (?)
                      81  -> pure $ E.EvKey (E.KFun   2) [] -- gnome-terminal
                      82  -> pure $ E.EvKey (E.KFun   3) [] -- gnome-terminal
                      83  -> pure $ E.EvKey (E.KFun   4) [] -- gnome-terminal
                      97  -> pure $ E.EvKey (E.KUp    1) [E.MCtrl] -- urxvt
                      98  -> pure $ E.EvKey (E.KDown  1) [E.MCtrl] -- urxvt
                      99  -> pure $ E.EvKey (E.KRight 1) [E.MCtrl] -- urxvt
                      100 -> pure $ E.EvKey (E.KLeft  1) [E.MCtrl] -- urxvt
                      xs  -> error (show xs)
      | x == 91   = wait >> getNextNonBlock >>= \case
                      Nothing -> pure $ E.EvKey (E.KChar [] '[') [E.MAlt] -- urxvt, gnome-terminal
                      Just y  -> decodeCSI y
      | x == 92   = pure $ E.EvKey (E.KChar [] '\\') [E.MAlt] -- urxvt, gnome-terminal
      | x == 93   = pure $ E.EvKey (E.KChar [] ']') [E.MAlt] -- urxvt, gnome-terminal
      | x == 94   = pure $ E.EvKey (E.KChar [] '6') [E.MAlt, E.MShift] -- urxvt, gnome-terminal
      | x == 95   = pure $ E.EvKey (E.KChar [] '_') [E.MAlt, E.MShift] -- urxvt, gnome-terminal
      | x == 96   = pure $ E.EvKey (E.KChar [] '`') [E.MAlt] -- urxvt, gnome-terminal
      | x >= 97 && x <= 122 = pure $ E.EvKey (E.KChar [] $ toEnum $ fromIntegral x) [E.MAlt]
      | x == 126  = pure $ E.EvKey (E.KChar [] '`') [E.MAlt, E.MShift] -- urxvt, gnome-terminal
      | x == 127  = pure $ E.EvKey E.KDelete []
      | otherwise = error $ show x

    decodeCSI :: (MonadInput m) => Word8 -> m E.Event
    decodeCSI y = withParams1 y $ \ps-> \case
      27         -> pure $ E.EvKey (E.KChar [] '[') [E.MAlt]          -- urxvt
      36         -> pure $ E.EvKey E.KDelete [E.MAlt, E.MShift]       -- urxvt, gnome-terminal
      64 {- @ -} -> withN 1 ps $ \n-> pure $ E.EvKey (E.KSpace  n) [] -- in urxvt shift+ctrl+pageup/down causes n==5/6
      65 {- A -} -> decodeArrowKey ps E.KUp
      66 {- B -} -> decodeArrowKey ps E.KDown
      67 {- C -} -> decodeArrowKey ps E.KRight
      68 {- D -} -> decodeArrowKey ps E.KLeft
      69 {- E -} -> undefined
      70 {- F -} -> pure $ E.EvKey E.KEnd []
      71 {- G -} -> undefined
      72 {- H -} -> pure $ E.EvKey E.KHome []
      73 {- I -} -> withN 1 ps $ \n-> pure $ E.EvKey (E.KTab    n) []
      74 {- J -} -> undefined
      75 {- K -} -> undefined
      76 {- L -} -> undefined
      77 {- M -} -> undefined
      78 {- N -} -> undefined
      79 {- O -} -> undefined
      80 {- P -} -> pure $ E.EvKey (E.KFun 1) [E.MCtrl]
      81 {- Q -} -> pure $ E.EvKey (E.KFun 2) [E.MCtrl]
      82 {- R -} -> pure $ E.EvKey (E.KFun 3) [E.MCtrl]
      83 {- S -} -> pure $ E.EvKey (E.KFun 4) [E.MCtrl]
      84 {- T -} -> undefined
      85 {- U -} -> undefined
      86 {- V -} -> undefined
      87 {- W -} -> undefined
      88 {- X -} -> undefined
      89 {- Y -} -> undefined
      90 {- Z -} -> withN 1 ps $ \n-> pure $ E.EvKey (E.KBackTab n) []
      94  -> case ps of
        [    50] {-    -} -> pure $ E.EvKey E.KInsert   [E.MCtrl]
        [    51] {-  3 -} -> pure $ E.EvKey E.KDelete   [E.MCtrl]
        [    53] {-  4 -} -> pure $ E.EvKey E.KPageUp   [E.MCtrl]
        [    54] {-  4 -} -> pure $ E.EvKey E.KPageDown [E.MCtrl]
        [    55] {-  6 -} -> pure $ E.EvKey E.KHome     [E.MCtrl]
        [    56] {-  6 -} -> pure $ E.EvKey E.KEnd      [E.MCtrl]
        [49, 49] {- 11 -} -> pure $ E.EvKey (E.KFun  1) [E.MCtrl]
        [49, 50] {- 12 -} -> pure $ E.EvKey (E.KFun  2) [E.MCtrl]
        [49, 51] {- 13 -} -> pure $ E.EvKey (E.KFun  3) [E.MCtrl]
        [49, 52] {- 14 -} -> pure $ E.EvKey (E.KFun  4) [E.MCtrl]
        [49, 53] {- 15 -} -> pure $ E.EvKey (E.KFun  5) [E.MCtrl]
        [49, 55] {- 17 -} -> pure $ E.EvKey (E.KFun  6) [E.MCtrl]
        [49, 56] {- 18 -} -> pure $ E.EvKey (E.KFun  7) [E.MCtrl]
        [49, 57] {- 19 -} -> pure $ E.EvKey (E.KFun  8) [E.MCtrl]
        [50, 48] {- 20 -} -> pure $ E.EvKey (E.KFun  9) [E.MCtrl]
        [50, 49] {- 21 -} -> pure $ E.EvKey (E.KFun 10) [E.MCtrl]
        [50, 51] {- 22 -} -> pure $ E.EvKey (E.KFun 11) [E.MCtrl]
        [50, 52] {- 23 -} -> pure $ E.EvKey (E.KFun 12) [E.MCtrl]
        [50, 53] {- 24 -} -> pure $ E.EvKey (E.KFun 13) [E.MCtrl]
        [50, 54] {- 25 -} -> pure $ E.EvKey (E.KFun 14) [E.MCtrl]
        [50, 56] {- 27 -} -> pure $ E.EvKey (E.KFun 15) [E.MCtrl]
        [50, 57] {- 28 -} -> pure $ E.EvKey (E.KFun 16) [E.MCtrl]
        [51, 49] {- 31 -} -> pure $ E.EvKey (E.KFun 17) [E.MCtrl]
        [51, 50] {- 32 -} -> pure $ E.EvKey (E.KFun 18) [E.MCtrl]
        [51, 51] {- 33 -} -> pure $ E.EvKey (E.KFun 19) [E.MCtrl]
        [51, 52] {- 32 -} -> pure $ E.EvKey (E.KFun 20) [E.MCtrl]
        _        -> error ("FOOB" ++ show ps)
      102 -> undefined
      105 -> pure $ E.EvKey E.KPrtScr []
      109 -> undefined -- SGR
      126 -> case ps of
        [    50]      {-  2 -} -> pure $ E.EvKey E.KInsert []
        [    51]      {-  3 -} -> pure $ E.EvKey E.KDelete []
        [    53]      {-  5 -} -> pure $ E.EvKey E.KPageUp []
        [    54]      {-  6 -} -> pure $ E.EvKey E.KPageDown []
        [    55]      {-  9 -} -> pure $ E.EvKey E.KHome []
        [    56]      {- 10 -} -> pure $ E.EvKey E.KEnd []
        [49, 49]      {- 11 -} -> pure $ E.EvKey (E.KFun 1) []
        [49, 50]      {- 12 -} -> pure $ E.EvKey (E.KFun 2) []
        [49, 51]      {- 13 -} -> pure $ E.EvKey (E.KFun 3) []
        [49, 52]      {- 14 -} -> pure $ E.EvKey (E.KFun 4) []
        [49, 53]      {- 15 -} -> pure $ E.EvKey (E.KFun 5) []
        [49, 55]      {- 17 -} -> pure $ E.EvKey (E.KFun 6) []
        [49, 56]      {- 18 -} -> pure $ E.EvKey (E.KFun 7) []
        [49, 57]      {- 19 -} -> pure $ E.EvKey (E.KFun 8) []
        [50, 48]      {- 20 -} -> pure $ E.EvKey (E.KFun 9) []
        [50, 49]      {- 21 -} -> pure $ E.EvKey (E.KFun 10) []
        [50, 51]      {- 22 -} -> pure $ E.EvKey (E.KFun 11) []
        [50, 52]      {- 23 -} -> pure $ E.EvKey (E.KFun 12) []
        [50, 53]      {- 24 -} -> pure $ E.EvKey (E.KFun 13) []
        [50, 54]      {- 25 -} -> pure $ E.EvKey (E.KFun 14) []
        [50, 56]      {- 27 -} -> pure $ E.EvKey (E.KFun 15) []
        [50, 57]      {- 28 -} -> pure $ E.EvKey (E.KFun 16) []
        [51, 49]      {- 31 -} -> pure $ E.EvKey (E.KFun 17) []
        [51, 50]      {- 32 -} -> pure $ E.EvKey (E.KFun 18) []
        [51, 51]      {- 33 -} -> pure $ E.EvKey (E.KFun 19) []
        [51, 52]      {- 32 -} -> pure $ E.EvKey (E.KFun 20) []
        [49,53,59,53] {- .. -} -> pure $ E.EvKey (E.KFun  5) [E.MCtrl] -- gnome-terminal
        [49,55,59,53] {- .. -} -> pure $ E.EvKey (E.KFun  6) [E.MCtrl] -- gnome-terminal
        [49,56,59,53] {- .. -} -> pure $ E.EvKey (E.KFun  7) [E.MCtrl] -- gnome-terminal
        [49,57,59,53] {- .. -} -> pure $ E.EvKey (E.KFun  8) [E.MCtrl] -- gnome-terminal
        [50,48,59,53] {- .. -} -> pure $ E.EvKey (E.KFun  9) [E.MCtrl] -- gnome-terminal
        [50,49,59,53] {- .. -} -> pure $ E.EvKey (E.KFun 10) [E.MCtrl] -- gnome-terminal
        [50,51,59,53] {- .. -} -> pure $ E.EvKey (E.KFun 11) [E.MCtrl] -- gnome-terminal
        [50,52,59,53] {- .. -} -> pure $ E.EvKey (E.KFun 12) [E.MCtrl] -- gnome-terminal
        [50,59,51]    {- .. -} -> pure $ E.EvKey E.KInsert [E.MAlt]    -- gnome-terminal
        [51,59,53]    {- .. -} -> pure $ E.EvKey E.KDelete [E.MCtrl]   -- xterm
        [51,59,51]    {- .. -} -> pure $ E.EvKey E.KDelete [E.MAlt]    -- gnome-terminal
        [53,59,53]    {- .. -} -> pure $ E.EvKey E.KPageUp [E.MCtrl]   -- gnome-terminal
        [54,59,53]    {- .. -} -> pure $ E.EvKey E.KPageDown [E.MCtrl]   -- gnome-terminal
        [53,59,51]    {- .. -} -> pure $ E.EvKey E.KPageUp [E.MAlt]   -- gnome-terminal
        [54,59,51]    {- .. -} -> pure $ E.EvKey E.KPageDown [E.MAlt]   -- gnome-terminal
        [53,59,55]    {- .. -} -> pure $ E.EvKey E.KPageUp [E.MCtrl, E.MAlt]   -- gnome-terminal
        [54,59,55]    {- .. -} -> pure $ E.EvKey E.KPageDown [E.MCtrl, E.MAlt]   -- gnome-terminal
        _             {- .. -} -> error $ show ps
      x   -> error $ "HERE" ++ show x
      where
        decodeArrowKey ps key = withNumbers ps $ \case
          []    -> pure $ E.EvKey (key 1) []
          [n]   -> pure $ E.EvKey (key $ if n == 0 then 1 else n) []
          [1,3] -> pure $ E.EvKey (key 1) [E.MAlt]          -- gnome-terminal
          [1,5] -> pure $ E.EvKey (key 1) [E.MCtrl]         -- gnome-terminal
          [1,7] -> pure $ E.EvKey (key 1) [E.MCtrl, E.MAlt] -- gnome-terminal
          xs    -> error (show xs)

unknownSequence :: (MonadInput m) => [Word8] -> m a
unknownSequence xs = getNextNonBlock >>= \case
  Just x  -> unknownSequence $ xs ++ [x]
  Nothing -> error $ "unknown sequence " ++ show xs

decodeSGR :: (MonadInput m) => [Word8] -> m SGR
decodeSGR []
        = pure Reset
decodeSGR (x:xs) = case x of
  0   -> pure $ Reset
  1   -> pure $ Bold
  2   -> pure $ undefined
  3   -> pure $ undefined
  30  -> pure $ SetForegroundColor $ Color4Bit Black   False
  31  -> pure $ SetForegroundColor $ Color4Bit Red     False
  32  -> pure $ SetForegroundColor $ Color4Bit Green   False
  33  -> pure $ SetForegroundColor $ Color4Bit Yellow  False
  34  -> pure $ SetForegroundColor $ Color4Bit Blue    False
  35  -> pure $ SetForegroundColor $ Color4Bit Magenta False
  36  -> pure $ SetForegroundColor $ Color4Bit Cyan    False
  37  -> pure $ SetForegroundColor $ Color4Bit White   False
  38  -> withColor xs $ pure . SetForegroundColor
  39  -> pure $ SetForegroundColor $ ColorDefault
  40  -> pure $ SetBackgroundColor $ Color4Bit Black   False
  41  -> pure $ SetBackgroundColor $ Color4Bit Black   False
  42  -> pure $ SetBackgroundColor $ Color4Bit Black   False
  43  -> pure $ SetBackgroundColor $ Color4Bit Black   False
  44  -> pure $ SetBackgroundColor $ Color4Bit Black   False
  45  -> pure $ SetBackgroundColor $ Color4Bit Black   False
  46  -> pure $ SetBackgroundColor $ Color4Bit Black   False
  47  -> pure $ SetBackgroundColor $ Color4Bit Black   False
  48  -> withColor xs $ pure . SetBackgroundColor
  49  -> pure $ SetBackgroundColor $ ColorDefault
  90  -> pure $ SetForegroundColor $ Color4Bit Black   True
  91  -> pure $ SetForegroundColor $ Color4Bit Black   True
  92  -> pure $ SetForegroundColor $ Color4Bit Black   True
  93  -> pure $ SetForegroundColor $ Color4Bit Black   True
  94  -> pure $ SetForegroundColor $ Color4Bit Black   True
  95  -> pure $ SetForegroundColor $ Color4Bit Black   True
  96  -> pure $ SetForegroundColor $ Color4Bit Black   True
  97  -> pure $ SetForegroundColor $ Color4Bit Black   True
  100 -> pure $ SetBackgroundColor $ Color4Bit Black   True
  101 -> pure $ SetBackgroundColor $ Color4Bit Black   True
  102 -> pure $ SetBackgroundColor $ Color4Bit Black   True
  103 -> pure $ SetBackgroundColor $ Color4Bit Black   True
  104 -> pure $ SetBackgroundColor $ Color4Bit Black   True
  105 -> pure $ SetBackgroundColor $ Color4Bit Black   True
  106 -> pure $ SetBackgroundColor $ Color4Bit Black   True
  107 -> pure $ SetBackgroundColor $ Color4Bit Black   True

withParams1 :: MonadInput m => Word8 -> ([Word8] -> Word8 -> m a) -> m a
withParams1 x f
  | x >= 0x30 && x <= 0x3f = withParameters 256 [x]
  | otherwise              = f [] x
  where
    withParameters 0 _            = fail "CSI: LENGTH LIMIT EXCEEDED"
    withParameters limit ps       = getNext >>= \case
      y | y >= 0x30 && y <= 0x3F -> withParameters (limit - 1) $! y:ps
        | otherwise              -> f (reverse ps) y

withN :: Monad m => Int -> [Word8] -> (Int -> m a) -> m a
withN defN [] f               = f defN
withN _    ps f               = g ps 0
  where
    g [] i                    = f i
    g (x:xs) i
      | x >= 48 && x <= 57    = g xs $! i * 256 - 48 + fromIntegral x
      | otherwise             = error $ "CSI: INVALID NUMBER " ++ show x

withNM :: Monad m => Int -> Int -> [Word8] -> (Int -> Int -> m a) -> m a
withNM defN defM []      f = f defN defM
withNM defN defM [59]    f = f defN defM
withNM defN defM (59:ps) f = withN defM ps (f defN)
withNM defN defM     ps  f = g ps 0
  where
    g [] i                 = fail "CSI: INVALID NUMBER"
    g (x:xs) i
      | x == 59            = withN defM xs (f i)
      | x >= 48 && x <= 57 = g xs $! i * 256 - 48 + fromIntegral x
      | otherwise          = fail "CSI: INVALID NUMBER"

withNumbers :: (Monad m, Integral n, Num n) => [Word8] -> ([n] -> m a) -> m a
withNumbers xs f = numbers' xs 0 >>= f
  where
    numbers' [] i
      = pure [i]
    numbers' (x:xs) i
      | x >= 48 && x <= 57 = numbers' xs (i * 256 - 48 + fromIntegral x)
      | x == 59            = (i:) <$> numbers' xs 0
      | otherwise          = fail ""

withColor :: (Monad n) => [Word8] -> (Color -> n a) -> n a
withColor xs f = withNumbers xs $ \case
  [5,n]     -> f $ Color8Bit n
  [2,r,g,b] -> f $ Color24Bit r g b
  _         -> fail ""

decodeUtf8Sequence :: MonadInput m => Word8 -> m Char
decodeUtf8Sequence x
  | x1                 < 0b10000000 = seq1
  | x1 .&. 0b11100000 == 0b11000000 = seq2
  | x1 .&. 0b11110000 == 0b11100000 = seq3
  | x1 .&. 0b11111000 == 0b11110000 = seq4
  | otherwise                       = reject
  where
    x1      = fromIntegral x :: Int
    reject  = pure '�'
    char c  = pure $ toEnum $ fromIntegral c
    seq1    = char x1
    seq2    = withNext $ \x2-> char $
      ((x1 .&. 0b00011111) `unsafeShiftL` 6) + x2
    seq3    = withNext $ \x2-> withNext $ \x3-> char $
      ((x1 .&. 0b00001111) `unsafeShiftL` 12) + (x2 `unsafeShiftL` 6) + x3
    seq4    = withNext $ \x2-> withNext $ \x3-> withNext $ \x4-> char $
      ((x1 .&. 0b00000111) `unsafeShiftL` 18) + (x2 `unsafeShiftL` 12) + (x3 `unsafeShiftL` 6) + x4
    withNext f = getNext >>= \case
      x | x .&. 0b11000000 == 0b10000000 -> f (fromIntegral x .&. 0b00111111)
        | otherwise                      -> reject

class Monad m => MonadInput m where
  getNext         :: m Word8
  getNextNonBlock :: m (Maybe Word8)
  wait            :: m ()

newtype InputT m a
  = InputT (StateT BS.ByteString m a)
  deriving (Functor, Applicative, Monad, MonadIO)

runInputT :: Monad m => InputT m a -> m a
runInputT (InputT m) = evalStateT m BS.empty

instance MonadIO m => MonadInput (InputT m) where
  getNext = InputT $ do
    bbs <- get
    case BS.uncons bbs of
      Just (b,bs) -> put bs >> pure b
      Nothing -> do
        ccs <- liftIO $ BS.hGetSome stdin 1024
        put (BS.tail ccs)
        pure (BS.head ccs)
  getNextNonBlock = InputT $ do
    bbs <- get
    case BS.uncons bbs of
      Just (b,bs) -> put bs >> pure (Just b)
      Nothing -> do
        ccs <- liftIO $ BS.hGetNonBlocking stdin 1024
        case BS.uncons ccs of
          Just (c,cs) -> put cs >> pure (Just c)
          Nothing     -> pure Nothing
  wait = InputT $ liftIO $ threadDelay 100000

withoutEcho :: IO a -> IO a
withoutEcho = bracket
  (hGetEcho stdin >>= \x-> hSetEcho stdin False >> pure x)
  (hSetEcho stdin) . const

withRawMode :: IO a -> IO a
withRawMode = bracket
  (hGetBuffering stdin >>= \b-> hSetBuffering stdin NoBuffering >> pure b)
  (hSetBuffering stdin) . const
