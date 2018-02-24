{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import qualified Control.Exception             as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Data.Char
import           Data.Function                 (fix)
import           Data.Monoid
import qualified Data.Text                     as Text
import qualified Data.Text.Prettyprint.Doc     as PP
import           System.Environment

import qualified Control.Monad.Repl            as R
import qualified Control.Monad.Terminal        as T
import           Control.Monad.Terminal.Ansi   as T
import qualified Control.Monad.Terminal.Events as T

import qualified System.Terminal.Ansi          as T

import           Control.Monad.Terminal

import           Prelude                       hiding (print, putChar)

type AnsiReplT s m = R.ReplT s (AnsiTerminalT m)

execAnsiReplT :: AnsiReplT s IO () -> s -> IO s
execAnsiReplT ma s = T.withTerminal $ T.runAnsiTerminalT (R.execReplT ma s)

evalAnsiReplT :: AnsiReplT s IO () -> s -> IO ()
evalAnsiReplT ma = void . execAnsiReplT ma

main :: IO ()
main = evalAnsiReplT (ini >> repl) 0
  where
    ini = R.setPrompt $ T.putDoc $ PP.annotate (T.bold True) $ (PP.annotate (T.foreground T.blue) "foo") <> "@bar % "

repl :: (T.MonadTerminal m, R.MonadRepl m, R.ReplState m ~ Int, MonadMask m) => m ()
repl = R.readString >>= \case
  Nothing -> R.quit
  Just s -> case s of
    ""         -> pure ()
    "quit"     -> R.quit
    "load"     -> R.load >>= R.print
    "inc"      -> R.load >>= R.store . succ
    "dec"      -> R.load >>= R.store . pred
    "loop"     -> R.print [1..]
    "cursor"   -> T.getCursorPosition >>= \xy-> R.print xy
    "progress" -> withProgressBar $ \update-> forM_ [1..100] $ \i-> do
                    threadDelay 100000
                    update $ fromIntegral i / 100
    line       -> R.print line

withProgressBar :: (T.MonadTerminal m, MonadMask m, Real p) => ((p -> IO ()) -> IO a) -> m a
withProgressBar action = do
  progress  <- liftIO $ newTVarIO 0
  progress' <- liftIO $ newTVarIO (-1)
  withAsyncM (action $ atomically . writeTVar progress) $ \asnc-> fix $ \loop->
    waitForInterruptEvent (\i-> waitInterrupt i `orElse` waitBackgroundThread asnc `orElse` waitProgress progress progress') >>= \case
      Right a -> renderProgress 1 >> pure a
      Left  p -> renderProgress p >> loop
  where
    waitInterrupt i = i >> throwSTM E.UserInterrupt
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
      putCr
      putString (padded ++ " % [")
      putText (Text.replicate x1 "=")
      when (p /= 1) (putChar '>')
      putText (Text.replicate x2 ".")
      putChar ']'
      flush

withAsyncM :: (MonadIO m, MonadMask m) => IO a -> (Async a -> m b) -> m b
withAsyncM ioa = bracket (liftIO $ async ioa) (liftIO . cancel)
