{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Terminal
  ( -- ** Printing
    -- ** MonadPrinter
    MonadPrinter (..)
    -- ** MonadFormatPrinter
  , MonadFormatPrinter (..)
    -- ** MonadPrettyPrinter
  , MonadPrettyPrinter (..)
    -- ** MonadColorPrinter
  , MonadColorPrinter (..)
  , Color (..)
  , ColorMode (..)
  , dull
  , bright
  , BasicColor (..)
    -- * Input Processing
    -- ** Event
  , Event (..)
  , Key (..)
  , Modifier (..)
  , MouseEvent (..)
    -- ** MonadEvent
  , MonadEvent (..)
  , waitEvent
  , pollEvent
    -- * Screen Manipulation
    -- ** MonadScreen
  , MonadScreen (..)
    -- * MonadTerminal
  , MonadTerminal (..)
  ) where

import           Control.Monad.Terminal.Events
import           Control.Monad.Terminal.Printer

class (MonadEvent m, MonadPrettyPrinter m, MonadFormatPrinter m, MonadColorPrinter m, MonadScreen m) => MonadTerminal m where

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
