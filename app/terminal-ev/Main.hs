module Main where

import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Catch       as E
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Function             (fix)

import           Data.Text.Prettyprint.Doc
import           System.Terminal

main :: IO ()
main = withTerminal $ runTerminalT $ fix $ \loop-> do
  waitEvent >>= \case
    Left Interrupt -> do
      putDocLn $ annotate (foreground red) "*** Interrupted"
      flush
    Right ev -> do
      case ev of
        OtherEvent {}         -> putDocLn $ annotate (foreground $ bright black)   (pretty $ show ev)
        KeyEvent (CharKey c) mods
          | isPrint c         -> putDocLn $ annotate (foreground $ bright blue)    (pretty $ "KeyEvent (CharKey '" ++ [c] ++ "') " ++ show mods)
          | otherwise         -> putDocLn $ annotate (foreground $ bright blue)    (pretty $ show ev)
        KeyEvent {}           -> putDocLn $ annotate (foreground $ bright blue)    (pretty $ show ev)
        WindowEvent {}        -> do
          sz <- getWindowSize
          putDocLn $ annotate (foreground $ bright magenta) (pretty $ show ev <> ": " <> show sz) 
        _ ->                     putDocLn $ pretty $ show ev
      flush
      loop
