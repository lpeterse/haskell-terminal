{-# LANGUAGE LambdaCase #-}
module System.Terminal.Class where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Maybe
import qualified Data.Text              as Text
import           Data.Word
import           Prelude                hiding (putChar)

import qualified System.Terminal.Color  as T
import qualified System.Terminal.Events as T
import qualified System.Terminal.Modes  as T
import qualified System.Terminal.Pretty as T

class (MonadEvent m, MonadIsolate m, MonadColorPrinter m, MonadScreen m) => MonadTerminal m where

class MonadIO m => MonadEvent m where
  getEvent     :: m T.Event
  getEvent      = withEventSTM id
  tryGetEvent  :: m (Maybe T.Event)
  tryGetEvent   = withEventSTM $ \e-> (Just <$> e) `orElse` pure Nothing
  withEventSTM :: (STM T.Event -> STM a) -> m a
  {-# MINIMAL withEventSTM #-}

class Monad m => MonadIsolate m where
  isolate            :: m a -> m a

class Monad m => MonadPrinter m where
  putLn              :: m ()
  putLn               = putChar '\n'
  putChar            :: Char -> m ()
  putString          :: String -> m ()
  putString           = mapM_ putChar
  putStringLn        :: String -> m ()
  putStringLn s       = putString s >> putLn
  putText            :: Text.Text -> m ()
  putText             = putString . Text.unpack
  putTextLn          :: Text.Text -> m ()
  putTextLn           = putStringLn . Text.unpack

  flush              :: m ()
  flush               = pure ()
  {-# MINIMAL putChar #-}

class MonadPrinter m => MonadColorPrinter m where
  -- ^ Reset all attributes including
  --
  --   * bold
  --   * color
  --   * positive/negative
  --   * underline
  --   * italic
  setDefault  :: m ()
  setForegroundColor :: T.Color -> m ()
  setBackgroundColor :: T.Color -> m ()
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

putDoc :: (MonadIsolate m, MonadColorPrinter m) => T.Doc -> m ()
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
