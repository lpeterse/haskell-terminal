{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Terminal
  (  -- * MonadTerminal
     MonadTerminal (..)
    -- ** Getting started
    -- * Printing
    -- ** MonadPrinter
  , MonadPrinter (..)
    -- ** MonadPrettyPrinter
  , MonadPrettyPrinter (..)
    -- ** MonadFormatPrinter
  , MonadFormatPrinter (..)
    -- ** MonadColorPrinter
  , MonadColorPrinter (..)
  , dull
  , bright
  , BasicColor (..)
  , Color (..)
  , ColorMode (..)
    -- * Input Processing
    -- ** MonadInput
  , MonadInput (..)
  , waitEvent
  , pollEvent
  , waitInterruptOrElse
    -- ** Events
  , Event (..)
  , Key (..)
  , Modifier (..)
  , MouseEvent (..)
    -- * Screen Manipulation
    -- ** MonadScreen
  , MonadScreen (..)
  ) where

import           Control.Monad.Terminal.Input
import           Control.Monad.Terminal.Printer

class (MonadInput m, MonadPrettyPrinter m, MonadFormatPrinter m, MonadColorPrinter m, MonadScreen m) => MonadTerminal m where

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
