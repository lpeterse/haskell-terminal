{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
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
import           Data.Ratio                  ((%))
import qualified Data.Text                   as Text
import qualified Data.Text.Prettyprint.Doc   as PP
import           Prelude                     hiding (putChar)

import           Control.Monad.Terminal

runWithProgressBar :: (MonadTerminal m, MonadMask m, Real p) => ((p -> IO ()) -> IO a) -> m (Maybe a)
runWithProgressBar action = do
  progress  <- liftIO $ newTVarIO 0
  progress' <- liftIO $ newTVarIO (-1)
  withAsyncM (action $ atomically . writeTVar progress) $ \asnc-> do
    hideCursor
    x <- fix $ \loop->
      waitInterruptOrElse (waitBackgroundThread asnc `orElse` waitProgress progress progress') >>= \case
        Nothing        -> render Yellow "Cancelling thread.." >> liftIO (cancel asnc) >> render Red "Thread cancelled." >> pure Nothing
        Just (Right a) -> renderProgress 1 >> pure (Just a)
        Just (Left  p) -> renderProgress p >> loop
    clearLine
    setHorizontalCursorPosition 0
    showCursor
    pure x
  where
    waitBackgroundThread asnc = Right <$> waitSTM asnc
    waitProgress progress progress' = readTVar progress >>= \p1-> swapTVar progress' p1 >>= \p2-> check (p1 > p2) >> pure (Left p1)
    renderProgress p = do
      width <- getLineWidth
      let x0 = width - 7
          x1 = max 0 $ min x0 $ truncate (fromIntegral x0 * toRational p)
          x2 = x0 - x1 :: Int
          x3 = truncate $ toRational p * 100 :: Int
          q0 = toRational p * toRational x0 :: Rational
          q1 = floor q0 % 1 :: Rational
          q2 = q0 - q1 :: Rational
          padded
            | x3 <   10 = "  " ++ show x3
            | x3 <  100 = " " ++ show x3
            | otherwise = "100"
      setHorizontalCursorPosition 0
      putString (padded ++ " % ")
      setAnnotation (foreground $ bright Black)
      putText (Text.replicate x1 "█")
      putChar $ if | q2 < 1 % 8 -> '▏'
                   | q2 < 2 % 8 -> '▎'
                   | q2 < 3 % 8 -> '▍'
                   | q2 < 4 % 8 -> '▌'
                   | q2 < 5 % 8 -> '▋'
                   | q2 < 6 % 8 -> '▊'
                   | q2 < 7 % 8 -> '▉'
                   | otherwise  -> '█'
      putText (Text.replicate x2 " ")
      resetAnnotations
      flush
    render color msg = do
      width <- getLineWidth
      setHorizontalCursorPosition 0
      putDoc $ PP.annotate (foreground $ bright color)
             $ PP.pretty $ msg <> Text.replicate (width - Text.length msg) " "
      flush

withAsyncM :: (MonadIO m, MonadMask m) => IO a -> (Async a -> m b) -> m b
withAsyncM ioa = bracket (liftIO $ async ioa) (liftIO . cancel)
