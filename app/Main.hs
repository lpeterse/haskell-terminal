{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Bits
import qualified Data.ByteString           as BS
import           Data.Word
import           System.IO
import qualified System.Terminal           as T

main :: IO ()
main = runInputT $ forever $
  decode >>= liftIO . print

decode :: MonadInput m => m InputSegment
decode = getNext >>= \case
  27 -> decodeEscape
  x  -> decodeUtf8Char1 (fromIntegral x)
  where
    decodeEscape = getNextNonBlock >>= \case
      Nothing -> do
        wait -- a single escape can only be distinguished by timing
        getNextNonBlock >>= \case
          Nothing -> pure "\\ESC"
          Just x  -> decodeEscape1 x
      Just x  -> decodeEscape1 x
    decodeEscape1 x
      | x < 97    = undefined
      | x < 123   = pure $ "ALT + " ++ [toEnum $ fromIntegral x]
      | otherwise = undefined

    decodeUtf8Char1 x1
      | x1                 < 0b10000000 = seq1
      | x1 .&. 0b11100000 == 0b11000000 = seq2
      | x1 .&. 0b11110000 == 0b11100000 = seq3
      | x1 .&. 0b11111000 == 0b11110000 = seq4
      | otherwise                       = reject
      where
        char c  = pure $ "CHAR " ++ [toEnum c :: Char]
        reject  = pure $ "CHAR " ++ ['�']
        seq1    = pure $ "CHAR " ++ [toEnum x1 :: Char]
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

type InputSegment = String

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
