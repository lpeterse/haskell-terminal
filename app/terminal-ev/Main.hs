module Main where

import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Catch       as E
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Function             (fix)

import           Control.Monad.Terminal
import           Data.Text.Prettyprint.Doc
import           System.Terminal

main :: IO ()
main = withTerminal $ runTerminalT $ fix $ \loop-> do
  ev <- waitEvent
  case ev of
    OtherEvent {}  -> putDocLn $ annotate (foreground $ bright Black)   (pretty $ show ev)
    KeyEvent (CharKey c) mods
      | isPrint c -> putDocLn $ annotate (foreground $ bright Blue) $ pretty $ "KeyEvent (CharKey '" ++ [c] ++ "') " ++ show mods
      | otherwise -> putDocLn $ annotate (foreground $ bright Blue) $ (pretty $ show ev)
    KeyEvent {}    -> putDocLn $ annotate (foreground $ bright Blue)    (pretty $ show ev)
    WindowEvent {} -> putDocLn $ annotate (foreground $ bright Magenta) (pretty $ show ev)
    _ ->              putDocLn $ pretty $ show ev
  flush
  when (ev == InterruptEvent) (E.throwM UserInterrupt)
  loop
