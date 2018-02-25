{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Terminal.Printer where

import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Word

import           Prelude                   hiding (putChar)

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
  putText            :: Text -> m ()
  putText             = putString . Data.Text.unpack
  putTextLn          :: Text -> m ()
  putTextLn           = putStringLn . Data.Text.unpack

  flush              :: m ()
  flush               = pure ()

  getLineWidth       :: m Int
  {-# MINIMAL putChar, getLineWidth #-}

class MonadPrinter m => MonadPrettyPrinter m where
  data Annotation m
  putDoc           :: Doc (Annotation m) -> m ()
  putDocLn         :: Doc (Annotation m) -> m ()
  putDocLn doc      = putDoc doc >> putLn
  setAnnotation    :: Annotation m -> m ()
  setAnnotation _   = pure ()
  unsetAnnotation  :: Annotation m -> m ()
  unsetAnnotation _ = pure ()
  resetAnnotations :: m ()
  resetAnnotations  = pure ()
  {-# MINIMAL putDoc | (putDoc, setAnnotation, unsetAnnotation, resetAnnotations) #-}

class MonadPrettyPrinter m => MonadFormatPrinter m where
  bold            :: Annotation m
  italic          :: Annotation m
  underlined      :: Annotation m

class MonadPrettyPrinter m => MonadColorPrinter m where
  inverted        :: Annotation m
  foreground      :: Color -> Annotation m
  background      :: Color -> Annotation m

data Color = Color ColorMode BasicColor
  deriving (Eq, Ord, Show)

data ColorMode
  = Dull
  | Bright
  deriving (Eq, Ord, Show)

data BasicColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Eq, Ord, Show)

dull :: BasicColor -> Color
dull = Color Dull

bright :: BasicColor -> Color
bright = Color Bright
