module System.Terminal.Output where

import           Prelude hiding (putStr)

data Color
  = Color Color8 Bool
  | ColorDefault
  deriving (Eq, Ord, Show)

data Color8
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Eq, Ord, Show)

class Monad m => MonadPrinter m where
  putString :: String -> m ()
  putStringLn :: String -> m ()
  nl     :: m ()
  cr     :: m ()
  flush  :: m ()
  reset  :: m ()
  setForegroundColor :: Color -> m ()
  setBackgroundColor :: Color -> m ()

class MonadPrinter m => MonadScreen m where

