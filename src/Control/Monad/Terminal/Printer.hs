{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Terminal.Printer where

import           Data.Text
import           Data.Text.Prettyprint.Doc

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
  resetAnnotations :: m ()
  resetAnnotations  = pure ()
  {-# MINIMAL putDoc | (putDoc, setAnnotation, resetAnnotations) #-}
