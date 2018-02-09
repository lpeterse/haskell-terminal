{-# LANGUAGE LambdaCase #-}
module System.Terminal.Class where

import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Bits
import qualified Data.ByteString           as BS
import           Data.Maybe
import           Data.Word
import           System.Environment
import           System.IO

import qualified System.Terminal.Events    as T
import qualified System.Terminal.Modes     as T
import qualified System.Terminal.Pretty    as T

class Monad m => MonadEvent m where
  getEvent :: m T.Event

class Monad m => MonadPrinter m where
  isolate            :: m a -> m a
  putChar            :: Char -> m ()
  putString          :: String -> m ()
  putStringLn        :: String -> m ()
  nl                 :: m ()
  cr                 :: m ()
  flush              :: m ()
  reset              :: m ()
  setForegroundColor :: T.Color -> m ()
  setBackgroundColor :: T.Color -> m ()
  -- ^ Reset all attributes including
  --
  --   * bold
  --   * color
  --   * positive/negative
  --   * underline
  --   * italic
  setDefault  :: m ()
  setUnderline :: Bool -> m ()
  setPositive :: m ()
  setNegative :: m ()

class MonadPrinter m => MonadScreen m where
  clear :: m ()
  cursorUp :: Int -> m ()
  cursorDown :: Int -> m ()
  cursorForward :: Int -> m ()
  cursorBackward :: Int -> m ()
  cursorPosition :: Int -> Int -> m ()
  cursorHide :: m ()
  cursorShow :: m ()

putDoc :: MonadPrinter m => T.Doc -> m ()
putDoc = \case
  T.Empty -> pure ()
  T.Char c -> putString [c]
  T.String s -> putString s
  T.Colored color doc -> isolate $ do
    setForegroundColor color
    putDoc doc
  T.Underlined doc -> isolate $ do
    setUnderline True
    putDoc doc
  T.Italic doc -> putDoc doc
  T.Bold doc -> putDoc doc
