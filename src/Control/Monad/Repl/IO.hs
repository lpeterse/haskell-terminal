{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Repl.IO where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Function               (fix)
import           Data.Monoid
import qualified Data.Text                   as Text
import qualified Data.Text.Prettyprint.Doc   as PP
import           Prelude                     hiding (putChar)

import           Control.Monad.Terminal

runWithProgressBar :: (MonadTerminal m, MonadMask m, Real p) => ((p -> IO ()) -> IO a) -> m (Maybe a)
runWithProgressBar action = do
  progress  <- liftIO $ newTVarIO 0
  progress' <- liftIO $ newTVarIO (-1)
  withAsyncM (action $ atomically . writeTVar progress) $ \asnc-> fix $ \loop->
    waitInterruptOrElse (waitBackgroundThread asnc `orElse` waitProgress progress progress') >>= \case
      Nothing        -> render Yellow "Cancelling thread.." >> liftIO (cancel asnc) >> render Red "Thread cancelled." >> pure Nothing
      Just (Right a) -> renderProgress 1 >> pure (Just a)
      Just (Left  p) -> renderProgress p >> loop
  where
    waitBackgroundThread asnc = Right <$> waitSTM asnc
    waitProgress progress progress' = readTVar progress >>= \p1-> swapTVar progress' p1 >>= \p2-> check (p1 > p2) >> pure (Left p1)
    renderProgress p = do
      width <- getLineWidth
      let x0 = width - (if p /= 1 then 9 else 8)
          x1 = max 0 $ min x0 $ truncate (fromIntegral x0 * toRational p)
          x2 = x0 - x1 :: Int
          x3 = truncate $ toRational p * 100 :: Int
          padded
            | x3 <   10 = "  " ++ show x3
            | x3 <  100 = " " ++ show x3
            | otherwise = "100"
      setHorizontalCursorPosition 0
      putString (padded ++ " % [")
      putText (Text.replicate x1 "=")
      when (p /= 1) (putChar '>')
      putText (Text.replicate x2 ".")
      putChar ']'
      flush
    render color msg = do
      width <- getLineWidth
      setHorizontalCursorPosition 0
      putDoc $ PP.annotate (foreground $ bright color)
             $ PP.pretty $ msg <> Text.replicate (width - Text.length msg) " "
      flush

withAsyncM :: (MonadIO m, MonadMask m) => IO a -> (Async a -> m b) -> m b
withAsyncM ioa = bracket (liftIO $ async ioa) (liftIO . cancel)
