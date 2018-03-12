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

repl :: (MonadTerminal m, MonadColorPrinter m, MonadMask m, MonadIO m) => ReplT Int m ()
repl = readString prompt >>= \case
    ""         -> pure ()
    "quit"     -> quit
    "fail"     -> fail "abcdef"
    "failIO"   -> liftIO $ E.throwIO $ E.userError "Exception thrown in IO."
    "throwM"   -> throwM $ E.userError "Exception thrown in ReplT."
    "liftThrowM" -> lift $ throwM $ E.userError "Exception thrown within the monad transformer."
    "load"     -> load >>= pprint
    "inc"      -> load >>= store . succ
    "dec"      -> load >>= store . pred
    "loop"     -> forM_ [1..100000] $ \i-> store i >> putString (' ':show i)
    "finally"  -> fail "I am failing, I am failing.." `finally` putStringLn "FINALLY"
    "cursor"   -> getCursorPosition >>= \(r,c)-> setCursorPosition (r - 3, c + 25)
    "progress" -> void $ runWithProgressBar $ \update-> (`finally` threadDelay 3000000) $ forM_ [1..100] $ \i-> do
                    threadDelay 100000
                    update $ fromIntegral i / 100
    "colors"   -> undefined
    line       -> putStringLn (show line)
