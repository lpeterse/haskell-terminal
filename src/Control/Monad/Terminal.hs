{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Terminal
  ( MonadPrinter (..)
  , MonadPrettyPrinter (..)
  , MonadColorPrinter (..)
  , MonadFormatPrinter (..)
  , MonadEvent (..)
  , waitEvent
  , pollEvent
  , MonadScreen (..)
  , MonadTerminal (..)
  , Color ()
  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  ) where

import           Control.Monad.Terminal.Events
import           Control.Monad.Terminal.Printer

class (MonadEvent m, MonadPrettyPrinter m,  MonadScreen m) => MonadTerminal m where

class MonadPrinter m => MonadScreen m where
  clear :: m ()
  cursorUp :: Int -> m ()
  cursorDown :: Int -> m ()
  cursorForward :: Int -> m ()
  cursorBackward :: Int -> m ()
  cursorPosition :: Int -> Int -> m ()
  cursorVisible :: Bool -> m ()
  getScreenSize :: m (Int,Int)
  getCursorPosition :: m (Int,Int)
