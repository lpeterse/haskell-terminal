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
import qualified Data.Text                   as Text
import qualified Data.Text.Prettyprint.Doc   as PP
import           GHC.Exts                    (oneShot)

import qualified Control.Monad.Repl          as R
import qualified Control.Monad.Repl.IO       as R
import           Control.Monad.Terminal
import           Data.Text.Prettyprint.Doc
import           System.Terminal

type AnsiReplT s m = R.ReplT s (AnsiTerminalT m)

runAnsiReplT :: AnsiReplT s IO () -> s -> IO s
runAnsiReplT ma s = withTerminal $ runAnsiTerminalT (R.runReplT ma s)

main :: IO ()
main = runAnsiReplT repl 0 >>= putStrLn . show

prompt :: (MonadFormatPrinter m, MonadColorPrinter m) => Doc (Annotation m)
prompt  = annotate bold $ annotate (foreground $ bright Blue) "repl" <> "@terminal % "

repl :: (MonadTerminal m, MonadColorPrinter m) => R.ReplT Int m ()
repl = R.readString prompt >>= \case
    ""       -> pure ()
    "quit"   -> R.quit
    --"load"     -> R.load >>= R.print
    --"inc"      -> R.load >>= R.store . succ
    --"dec"      -> R.load >>= R.store . pred
    "loop"   -> oneShot gnurp 100000
    "cursor" -> getCursorPosition >>= \xy-> R.print xy
    --"progress" -> void $ R.runWithProgressBar $ \update-> (`finally` threadDelay 3000000) $ forM_ [1..100] $ \i-> do
    --                threadDelay 100000
    --                update $ fromIntegral i / 100
    "colors" -> undefined
    line     -> putStringLn (show line)

gnurp :: MonadPrinter m => Int -> m ()
gnurp i = putString (show [1..i])
{-# NOINLINE gnurp #-}
