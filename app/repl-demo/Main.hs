{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import qualified Control.Exception           as E
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Data.Char
import           Data.Function               (fix)
import           Data.Monoid
import qualified Data.Text                   as Text
import qualified Data.Text.Prettyprint.Doc   as PP
import           Prelude                     hiding (print, putChar)
import           System.Environment

import qualified Control.Monad.Repl          as R
import qualified Control.Monad.Repl.IO       as R
import           Control.Monad.Terminal
import           System.Terminal.Ansi

type AnsiReplT s m = R.ReplT s (AnsiTerminalT m)

execAnsiReplT :: AnsiReplT s IO () -> s -> IO s
execAnsiReplT ma s = withTerminal $ runAnsiTerminalT (R.execReplT ma s)

evalAnsiReplT :: AnsiReplT s IO () -> s -> IO ()
evalAnsiReplT ma = void . execAnsiReplT ma

main :: IO ()
main = evalAnsiReplT (ini >> repl) 0
  where
    ini = R.setPrompt $ putDoc $ PP.annotate bold $ (PP.annotate (foreground $ bright Blue) "foo") <> "@bar % "

repl :: (MonadTerminal m, MonadColorPrinter m, R.MonadRepl m, R.ReplState m ~ Int, MonadMask m) => m ()
repl = R.readString >>= \case
  Nothing -> R.quit
  Just s -> case s of
    ""         -> pure ()
    "quit"     -> R.quit
    "load"     -> R.load >>= R.print
    "inc"      -> R.load >>= R.store . succ
    "dec"      -> R.load >>= R.store . pred
    "loop"     -> putString (show [1..])
    "cursor"   -> getCursorPosition >>= \xy-> R.print xy
    "progress" -> void $ R.runWithProgressBar $ \update-> (`finally` threadDelay 3000000) $ forM_ [1..100] $ \i-> do
                    threadDelay 100000
                    update $ fromIntegral i / 100
    "colors"   -> printColors
    line       -> R.print line

printColors ::  (MonadColorPrinter m) => m ()
printColors = do
  putDocLn doc
  where
    doc = PP.annotate (foreground $ bright Yellow) (" yellow " <> PP.annotate (foreground $ dull Red) " red " <> " yellow ")
