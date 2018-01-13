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
main = withoutEcho $ withRawMode $ runInputT $ forever $
  decodeAnsi >>= liftIO . print

decodeAnsi :: MonadInput m => m E.Event
decodeAnsi = decode1 =<< getNext
  where
    decode1 :: MonadInput m => Word8 -> m E.Event
    decode1 x
      | x == 27   = decodeEscape
      | x <= 31   = pure $ E.EvKey (E.KChar $ toEnum $ 64 + fromIntegral x) [E.MCtrl]
      | x == 127  = pure $ E.EvKey E.KBackspace []
      | otherwise = flip E.EvKey [] . E.KChar <$> decodeUtf8Sequence x

    decodeEscape :: (MonadInput m) => m E.Event
    decodeEscape = getNextNonBlock >>= \case
      Nothing -> do
        wait -- a single escape can only be distinguished by timing
        getNextNonBlock >>= \case
          Nothing -> pure $ E.EvKey (E.KChar '[') [E.MCtrl]
          Just x  -> decodeEscapeSequence x
      Just x  -> decodeEscapeSequence x

    decodeEscapeSequence :: (MonadInput m) => Word8 -> m E.Event
    decodeEscapeSequence x
      | x == 8    = pure $ E.EvKey E.KBackspace []
      | x == 10   = pure $ E.EvKey (E.KChar '\n') [E.MAlt]
      | x == 27   = getNext >>= \x-> getNext >>= \y-> getNext >>= \z-> error (show  [x,y,z])-- pure $ E.EvKey E.KEsc []
      | x == 91   = decodeCSI
      | x < 97    = error $ show x
      | x < 123   = decode1 x
      | x == 127  = pure $ E.EvKey E.KDelete []
      | otherwise = error $ show x

    decodeCSI :: (MonadInput m) => m E.Event
    decodeCSI = withParams $ \ps-> \case
      65  -> pure $ E.EvKey E.KUp []
      66  -> pure $ E.EvKey E.KDown []
      67  -> pure $ E.EvKey E.KRight []
      68  -> pure $ E.EvKey E.KLeft []
      69  -> undefined -- pure $ E.Event $ E.EvKey E.KDownLeft   -- cursor next line
      70  -> undefined -- pure $ E.Event $ E.EvKey E.KUpLeft     -- cursor previous line
      71  -> undefined -- withN 1 ps (pure . CursorHorizontalAbsolute)
      72  -> undefined -- withNM 1 1 ps ((pure .) . CursorPosition)
      73  -> undefined
      74  -> undefined -- withN 0 ps (pure . EraseInDisplay)
      75  -> undefined -- withN 0 ps (pure . EraseInLine)
      76  -> undefined
      77  -> undefined
      78  -> undefined
      79  -> undefined
      80  -> undefined
      81  -> undefined
      82  -> undefined
      83  -> undefined
      84  -> undefined
      102 -> undefined -- withNM 1 1 ps ((pure .) .  HorizontalVerticalPosition)
      109 -> undefined -- SGR <$> decodeSGR ps
      126 -> case ps of
        [    50] {-  2 -} -> pure $ E.EvKey E.KIns []
        [    51] {-  3 -} -> pure $ E.EvKey E.KDelete []
        [    53] {-  5 -} -> pure $ E.EvKey E.KPageUp []
        [    54] {-  6 -} -> pure $ E.EvKey E.KPageDown []
        [    55] {-  9 -} -> undefined -- pure Pos1
        [    56] {- 10 -} -> undefined -- pure End
        [49, 49] {- 11 -} -> pure $ E.EvKey (E.KFun 1) []
        [49, 50] {- 12 -} -> pure $ E.EvKey (E.KFun 2) []
        [49, 51] {- 13 -} -> pure $ E.EvKey (E.KFun 3) []
        [49, 52] {- 14 -} -> pure $ E.EvKey (E.KFun 4) []
        [49, 53] {- 15 -} -> pure $ E.EvKey (E.KFun 5) []
        [49, 55] {- 17 -} -> pure $ E.EvKey (E.KFun 6) []
        [49, 56] {- 18 -} -> pure $ E.EvKey (E.KFun 7) []
        [49, 57] {- 19 -} -> pure $ E.EvKey (E.KFun 8) []
        [50, 48] {- 20 -} -> pure $ E.EvKey (E.KFun 9) []
        [50, 49] {- 21 -} -> pure $ E.EvKey (E.KFun 10) []
        [50, 51] {- 22 -} -> pure $ E.EvKey (E.KFun 11) []
        [50, 52] {- 23 -} -> pure $ E.EvKey (E.KFun 12) []
        [50, 53] {- 24 -} -> pure $ E.EvKey (E.KFun 13) []
        [50, 54] {- 25 -} -> pure $ E.EvKey (E.KFun 14) []
        [50, 56] {- 27 -} -> pure $ E.EvKey (E.KFun 15) []
        [50, 57] {- 28 -} -> pure $ E.EvKey (E.KFun 16) []
        [51, 49] {- 31 -} -> pure $ E.EvKey (E.KFun 17) []
        [51, 50] {- 32 -} -> pure $ E.EvKey (E.KFun 18) []
        [51, 51] {- 33 -} -> pure $ E.EvKey (E.KFun 19) []
        [51, 52] {- 32 -} -> pure $ E.EvKey (E.KFun 20) []
        _        -> error $ show ps
      90  -> pure $ E.EvKey E.KBackTab []
      105 -> pure $ E.EvKey E.KPrtScr []
      x   -> error $ show x

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

withParams :: MonadInput m => ([Word8] -> Word8 -> m a) -> m a
withParams f = withParameters 256 []
  where
    withParameters 0 _            = fail "CSI: LENGTH LIMIT EXCEEDED"
    withParameters limit ps       = getNext >>= \case
      x | x >= 0x30 && x <= 0x3F -> withParameters (limit - 1) $! x:ps
        | otherwise              -> f (reverse ps) x

withN :: Monad m => Int -> [Word8] -> (Int -> m a) -> m a
withN defN [] f               = f defN
withN _    ps f               = g ps 0
  where
    g [] i                    = f i
    g (x:xs) i
      | x >= 48 && x <= 57    = g xs $! i * 256 - 48 + fromIntegral x
      | otherwise             = fail "CSI: INVALID NUMBER"

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
