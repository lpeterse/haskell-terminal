{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Control.Monad.Terminal.Ansi.AnsiTerminalT
  ( AnsiTerminalT ()
  , runAnsiTerminalT
  )
where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import qualified Control.Exception                        as E
import           Control.Monad                            (forever, void, when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bits
import qualified Data.ByteString                          as BS
import           Data.Char
import           Data.Function                            (fix)
import           Data.List.NonEmpty                       (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                       as N
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                                as Text
import qualified Data.Text.IO                             as Text
import qualified Data.Text.Prettyprint.Doc                as PP
import           Data.Word
import           System.Environment
import qualified System.IO                                as IO

import qualified Control.Monad.Terminal                   as T
import qualified Control.Monad.Terminal.Ansi.AnsiTerminal as T
import qualified Control.Monad.Terminal.Ansi.Decoder      as T
import qualified Control.Monad.Terminal.Events            as T
import qualified Control.Monad.Terminal.Printer           as T

import qualified System.Terminal.Ansi.Platform            as T

newtype AnsiTerminalT m a
  = AnsiTerminalT (ReaderT T.AnsiTerminal m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runAnsiTerminalT :: (MonadIO m, MonadMask m) => AnsiTerminalT m a -> T.AnsiTerminal -> m a
runAnsiTerminalT (AnsiTerminalT action) ansi =
  runReaderT action ansi { T.ansiInputEvents = events }
  where
    events = (mapEvent <$> runReaderT T.decodeAnsi (T.ansiInputChars ansi)) `orElse` T.ansiInputEvents ansi
    mapEvent ev@(T.EvKey (T.KChar c) [])
      | c == '\NUL' = T.EvKey T.KNull []
      | c  < ' '    = fromMaybe (T.EvKey (T.KChar $ toEnum $ 64 + fromEnum c) [T.MCtrl]) (T.ansiSpecialChars ansi c)
      | otherwise   = fromMaybe ev (T.ansiSpecialChars ansi c)
    mapEvent ev = ev

instance MonadTrans AnsiTerminalT where
  lift = AnsiTerminalT . lift

instance (MonadIO m) => T.MonadTerminal (AnsiTerminalT m) where

instance (MonadIO m) => T.MonadEvent (AnsiTerminalT m) where
  waitForEvent f = AnsiTerminalT $ do
    ansi <- ask
    liftIO $ atomically $ do
      void (T.ansiInterrupt ansi) -- Reset the interrupt flag. State is not relevant.
      f (T.ansiInputEvents ansi)  -- Apply user transformation on event source.
  waitForInterruptEvent f = AnsiTerminalT $ do
    ansi <- ask
    liftIO $ atomically $ f $ T.ansiInterrupt ansi >>= check

instance (MonadIO m) => T.MonadPrinter (AnsiTerminalT m) where
  putChar c = AnsiTerminalT $ do
    ansi <- ask
    liftIO $ atomically $ T.ansiOutput ansi $ Text.singleton c
  putString = \case
    [] -> pure ()
    (x:xs) -> T.putChar x >> T.putString xs
  putText t = AnsiTerminalT $ do
    ansi <- ask
    liftIO $ atomically $ T.ansiOutput ansi t
  flush = AnsiTerminalT $ do
    ansi <- ask
    liftIO  $ atomically $ T.ansiOutputFlush ansi
  getLineWidth = snd <$> T.getScreenSize

instance (MonadIO m) => T.MonadPrettyPrinter (AnsiTerminalT m) where
  data Annotation (AnsiTerminalT m)
    = Bold
    | Italic
    | Underlined
    | Inverted
    | Foreground T.Color
    | Background T.Color deriving (Eq, Ord, Show)
  putDocLn doc = T.putDoc doc >> T.putLn
  putDoc doc = do
    w <- T.getLineWidth
    T.resetAnnotations
    render [] (sdoc w)
    T.resetAnnotations
    T.flush
    where
      options w   = PP.defaultLayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine w 1.0 }
      sdoc w      = PP.layoutSmart (options w) doc
      oldFG []                = Nothing
      oldFG (Foreground c:xs) = Just c
      oldFG (_:xs)            = oldFG xs
      oldBG []                = Nothing
      oldBG (Background c:xs) = Just c
      oldBG (_:xs)            = oldBG xs
      render anns = \case
        PP.SFail           -> pure ()
        PP.SEmpty          -> pure ()
        PP.SChar c ss      -> T.putChar c >> render anns ss
        PP.SText _ t ss    -> T.putText t >> render anns ss
        PP.SLine n ss      -> T.putLn >> T.putText (Text.replicate n " ") >> render anns ss
        PP.SAnnPush ann ss -> T.setAnnotation ann >> render (ann:anns) ss
        PP.SAnnPop ss      -> case anns of
          []                     -> render [] ss
          (Bold         :anns')
            | Bold       `elem` anns' -> pure ()
            | otherwise               -> T.unsetAnnotation Bold       >> render anns' ss
          (Italic       :anns')
            | Italic     `elem` anns' -> pure ()
            | otherwise               -> T.unsetAnnotation Italic     >> render anns' ss
          (Underlined   :anns')
            | Underlined `elem` anns' -> pure ()
            | otherwise               -> T.unsetAnnotation Underlined >> render anns' ss
          (Inverted     :anns')
            | Inverted   `elem` anns' -> pure ()
            | otherwise               -> T.unsetAnnotation Inverted   >> render anns' ss
          (Foreground c :anns') -> case oldFG anns' of
            Just d  -> T.setAnnotation   (Foreground d) >> render anns' ss
            Nothing -> T.unsetAnnotation (Foreground c) >> render anns' ss
          (Background c :anns') -> case oldBG anns' of
            Just d  -> T.setAnnotation   (Background d) >> render anns' ss
            Nothing -> T.unsetAnnotation (Background c) >> render anns' ss

  setAnnotation Bold                                           = write "\ESC[1m"
  setAnnotation Italic                                         = pure ()
  setAnnotation Underlined                                     = write "\ESC[4m"
  setAnnotation Inverted                                       = write "\ESC[7m"
  setAnnotation (Foreground c@(T.Color4Bit T.Black     False)) = write "\ESC[30m"
  setAnnotation (Foreground c@(T.Color4Bit T.Red       False)) = write "\ESC[31m"
  setAnnotation (Foreground c@(T.Color4Bit T.Green     False)) = write "\ESC[32m"
  setAnnotation (Foreground c@(T.Color4Bit T.Yellow    False)) = write "\ESC[33m"
  setAnnotation (Foreground c@(T.Color4Bit T.Blue      False)) = write "\ESC[34m"
  setAnnotation (Foreground c@(T.Color4Bit T.Magenta   False)) = write "\ESC[35m"
  setAnnotation (Foreground c@(T.Color4Bit T.Cyan      False)) = write "\ESC[36m"
  setAnnotation (Foreground c@(T.Color4Bit T.White     False)) = write "\ESC[37m"
  setAnnotation (Foreground c@(T.Color4Bit T.Black      True)) = write "\ESC[90m"
  setAnnotation (Foreground c@(T.Color4Bit T.Red        True)) = write "\ESC[91m"
  setAnnotation (Foreground c@(T.Color4Bit T.Green      True)) = write "\ESC[92m"
  setAnnotation (Foreground c@(T.Color4Bit T.Yellow     True)) = write "\ESC[93m"
  setAnnotation (Foreground c@(T.Color4Bit T.Blue       True)) = write "\ESC[94m"
  setAnnotation (Foreground c@(T.Color4Bit T.Magenta    True)) = write "\ESC[95m"
  setAnnotation (Foreground c@(T.Color4Bit T.Cyan       True)) = write "\ESC[96m"
  setAnnotation (Foreground c@(T.Color4Bit T.White      True)) = write "\ESC[97m"
  setAnnotation (Foreground _                                ) = error "FIXME"
  setAnnotation (Background c@(T.Color4Bit T.Black     False)) = write "\ESC[40m"
  setAnnotation (Background c@(T.Color4Bit T.Red       False)) = write "\ESC[41m"
  setAnnotation (Background c@(T.Color4Bit T.Green     False)) = write "\ESC[42m"
  setAnnotation (Background c@(T.Color4Bit T.Yellow    False)) = write "\ESC[43m"
  setAnnotation (Background c@(T.Color4Bit T.Blue      False)) = write "\ESC[44m"
  setAnnotation (Background c@(T.Color4Bit T.Magenta   False)) = write "\ESC[45m"
  setAnnotation (Background c@(T.Color4Bit T.Cyan      False)) = write "\ESC[46m"
  setAnnotation (Background c@(T.Color4Bit T.White     False)) = write "\ESC[47m"
  setAnnotation (Background c@(T.Color4Bit T.Black      True)) = write "\ESC[100m"
  setAnnotation (Background c@(T.Color4Bit T.Red        True)) = write "\ESC[101m"
  setAnnotation (Background c@(T.Color4Bit T.Green      True)) = write "\ESC[102m"
  setAnnotation (Background c@(T.Color4Bit T.Yellow     True)) = write "\ESC[103m"
  setAnnotation (Background c@(T.Color4Bit T.Blue       True)) = write "\ESC[104m"
  setAnnotation (Background c@(T.Color4Bit T.Magenta    True)) = write "\ESC[105m"
  setAnnotation (Background c@(T.Color4Bit T.Cyan       True)) = write "\ESC[106m"
  setAnnotation (Background c@(T.Color4Bit T.White      True)) = write "\ESC[107m"
  setAnnotation (Background _                                ) = error "FIXME"
  unsetAnnotation Bold           = write "\ESC[22m"
  unsetAnnotation Italic         = pure ()
  unsetAnnotation Underlined     = write "\ESC[24m"
  unsetAnnotation Inverted       = write "\ESC[27m"
  unsetAnnotation (Foreground _) = write "\ESC[39m"
  unsetAnnotation (Background _) = write "\ESC[49m"
  resetAnnotations               = write "\ESC[m"

instance (MonadIO m) => T.MonadFormatPrinter (AnsiTerminalT m) where
  bold       = Bold
  italic     = Italic
  underlined = Underlined

instance (MonadIO m) => T.MonadColorPrinter (AnsiTerminalT m) where
  inverted   = Inverted
  foreground = Foreground
  background = Background

instance (MonadIO m) => T.MonadScreen (AnsiTerminalT m) where
  clear                                           = write "\ESC[H"
  cursorUp i                                      = write $ "\ESC[" <> Text.pack (show i) <> "A"
  cursorDown i                                    = write $ "\ESC[" <> Text.pack (show i) <> "B"
  cursorForward i                                 = write $ "\ESC[" <> Text.pack (show i) <> "C"
  cursorBackward i                                = write $ "\ESC[" <> Text.pack (show i) <> "D"
  cursorPosition x y                              = write $ "\ESC[" <> Text.pack (show x) <> ";" <> Text.pack (show y) <> "H"
  cursorVisible                             False = write "\ESC[?25l"
  cursorVisible                              True = write "\ESC[?25h"
  --askCursorPosition                               = write "\ESC[6n"
  getCursorPosition                               = pure (0,0)
  getScreenSize = AnsiTerminalT $ do
    ansi <- ask
    liftIO $ atomically $ T.ansiScreenSize ansi

write :: (MonadIO m) => Text.Text -> AnsiTerminalT m ()
write t = AnsiTerminalT $ do
  ansi <- ask
  liftIO $ atomically $ T.ansiOutput ansi t