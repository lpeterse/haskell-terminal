{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
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
  x  -> undefined
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
