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

data Color
  = Color4Bit  Color3Bit Bool
  | Color8Bit  Word8
  | Color24bit Word8 Word8 Word8
  deriving (Eq, Ord, Show)

data Color3Bit
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Eq, Ord, Show)

rgb :: Word8 -> Word8 -> Word8 -> Color
rgb  = Color24bit

black :: Color
black = Color4Bit Black False

red :: Color
red = Color4Bit Red False

green :: Color
green = Color4Bit Green False

yellow :: Color
yellow  = Color4Bit Yellow False

blue :: Color
blue = Color4Bit Blue False

magenta :: Color
magenta = Color4Bit Magenta False

cyan :: Color
cyan = Color4Bit Cyan False

white :: Color
white = Color4Bit White False
