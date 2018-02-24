{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Terminal where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Maybe
import qualified Data.Text                     as Text
import qualified Data.Text.Prettyprint.Doc     as Pretty
import           Data.Word
import           Prelude                       hiding (putChar)

import qualified Control.Monad.Terminal.Color  as T
import qualified Control.Monad.Terminal.Events as T
import qualified Control.Monad.Terminal.Modes  as T

class (MonadPrettyPrinter m, MonadAnsiPrinter m, MonadEvent m, MonadScreen m) => MonadTerminal m where

class MonadIO m => MonadEvent m where
  waitForEvent          :: (STM T.Event -> STM a) -> m a
  waitForInterruptEvent :: (STM () -> STM a) -> m a

getEvent     :: MonadEvent m => m T.Event
getEvent      = waitForEvent id

tryGetEvent  :: MonadEvent m => m (Maybe T.Event)
tryGetEvent   = waitForEvent $ \ev-> (Just <$> ev) `orElse` pure Nothing

class Monad m => MonadPrinter m where
  putLn              :: m ()
  putLn               = putChar '\n'
  putCr              :: m ()
  putCr               = putChar '\r'
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

  getLineWidth       :: m Int
  {-# MINIMAL putChar, getLineWidth #-}

class MonadPrinter m => MonadPrettyPrinter m where
  data Annotation m
  putDoc           :: Pretty.Doc (Annotation m) -> m ()
  putDocLn         :: Pretty.Doc (Annotation m) -> m ()
  putDocLn doc      = putDoc doc >> putLn
  setAnnotation    :: Annotation m -> m ()
  setAnnotation _   = pure ()
  resetAnnotations :: m ()
  resetAnnotations  = pure ()
  {-# MINIMAL putDoc | (putDoc, setAnnotation, resetAnnotations) #-}

class MonadPrettyPrinter m => MonadAnsiPrinter m where
  bold            :: Bool    -> Annotation m
  inverted        :: Bool    -> Annotation m
  underlined      :: Bool    -> Annotation m
  foreground      :: T.Color -> Annotation m
  background      :: T.Color -> Annotation m

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
