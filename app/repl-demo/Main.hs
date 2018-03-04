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

import           Control.Monad.Repl
import           Control.Monad.Repl.IO
import           Control.Monad.Terminal
import           Data.Text.Prettyprint.Doc
import qualified System.IO.Error             as E
import           System.Terminal

type AnsiReplT s m = ReplT s (AnsiTerminalT m)

runAnsiReplT :: AnsiReplT s IO () -> s -> IO s
runAnsiReplT ma s = withTerminal $ runAnsiTerminalT (runReplT ma s)

main :: IO ()
main = runAnsiReplT repl 0 >>= Prelude.print

prompt :: (MonadFormatPrinter m, MonadColorPrinter m) => Doc (Annotation m)
prompt  = annotate bold $ annotate (foreground $ bright Blue) "repl" <> "@terminal % "

repl :: (MonadTerminal m, MonadColorPrinter m) => ReplT Int m ()
repl = readString prompt >>= \case
    ""         -> pure ()
    "quit"     -> quit
    "fail"     -> fail "abcdef"
    "failIO"   -> liftIO $ E.throwIO $ E.userError "Exception thrown in IO."
    "load"     -> load >>= pprint
    "inc"      -> load >>= store . succ
    "dec"      -> load >>= store . pred
    "loop"     -> gnurp 100000
    "cursor"   -> getCursorPosition >>= \xy-> pprint xy
    --"progress" -> void $ runWithProgressBar $ \update-> (`finally` threadDelay 3000000) $ forM_ [1..100] $ \i-> do
    --                threadDelay 100000
    --                update $ fromIntegral i / 100
    "colors"   -> undefined
    line       -> putStringLn (show line)

gnurp :: MonadPrinter m => Int -> m ()
gnurp i = putString (show [1..i])
{-# NOINLINE gnurp #-}

