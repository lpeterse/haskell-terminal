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

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception                   as E
import           Control.Monad                       (when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Foldable                       (forM_)
import           Data.Monoid
import qualified Data.Text                           as Text
import qualified Data.Text.Prettyprint.Doc           as PP

import qualified Control.Monad.Terminal              as T
import qualified Control.Monad.Terminal.Ansi.Decoder as T

newtype AnsiTerminalT m a
  = AnsiTerminalT (ReaderT T.Terminal m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

runAnsiTerminalT :: (MonadIO m, MonadMask m) => AnsiTerminalT m a -> T.Terminal -> m a
runAnsiTerminalT (AnsiTerminalT action) ansi = do
  eventsChan <- liftIO newTChanIO
  decoderVar <- liftIO (newTVarIO T.ansiDecoder)
  runReaderT action ansi { T.termInput = nextEvent eventsChan decoderVar }
  where
    -- This function transforms the incoming raw event stream and sends
    -- parts of it (all plain characters) through the ANSI decoder.
    -- It does this in a transactional way whereas it is important that
    -- each input event creates _at least one_ immediate output event.
    -- This means: When feeding input characters to the decoder it
    -- is necessary to emit one intermediate event after each character that
    -- has been fed into the decoder. Otherwise the transaction
    -- will fail and the actual modification to the decoder won't happen.
    nextEvent eventsChan decoderVar = do
      processRawEvent `orElse` pure () -- Even without a new raw event there might be events pending.
      readTChan eventsChan
      where
        processRawEvent = T.termInput ansi >>= \case
            T.KeyEvent (T.CharKey c) mods
              | mods == mempty -> do
                  -- NB: This event is essential as it guarantees the whole transaction
                  -- to succeed by having at least one event in the `events` channel.
                  -- This information is also quite nice for debbuging.
                  writeTChan eventsChan (T.OtherEvent $ "ANSI decoder: Feeding character " ++ show c ++ ".")
                  -- Feed the decoder, save its new state and write its output to the
                  -- events chan (if any).
                  decoder <- readTVar decoderVar
                  let (ansiEvents, decoder') = T.feedDecoder decoder c
                  writeTVar decoderVar decoder'
                  forM_ ansiEvents (writeTChan eventsChan)
            event -> writeTChan eventsChan event

instance MonadTrans AnsiTerminalT where
  lift = AnsiTerminalT . lift

instance (MonadIO m) => T.MonadInput (AnsiTerminalT m) where
  waitMapInterruptAndEvents f = AnsiTerminalT $ do
    ansi <- ask
    liftIO $ atomically $ f (T.termInterrupt ansi) (T.termInput ansi)

instance (MonadIO m, MonadThrow m) => T.MonadPrinter (AnsiTerminalT m) where
  putChar c = AnsiTerminalT $ do
    ansi <- ask
    when (safeChar c) $
      liftIO $ atomically $ T.termOutput ansi $! Text.singleton c
  putString cs = AnsiTerminalT $ do
    ansi <- ask
    liftIO $ forM_ (filter safeChar cs) $ \c->
      atomically $ T.termOutput ansi $! Text.singleton c
  putText t = AnsiTerminalT $ do
    ansi <- ask
    liftIO $ loop (atomically . T.termOutput ansi) (Text.filter safeChar t)
    where
      loop out t0
        | Text.null t0 = pure ()
        | otherwise    = let (t1,t2) = Text.splitAt 80 t0
                         in  out t1 >> loop out t2
  flush = AnsiTerminalT $ do
    ansi <- ask
    liftIO  $ atomically $ T.termFlush ansi
  getLineWidth = snd <$> T.getScreenSize

instance (MonadIO m, MonadThrow m) => T.MonadPrettyPrinter (AnsiTerminalT m) where
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
      oldFG []               = Nothing
      oldFG (Foreground c:_) = Just c
      oldFG (_:xs)           = oldFG xs
      oldBG []               = Nothing
      oldBG (Background c:_) = Just c
      oldBG (_:xs)           = oldBG xs
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
            | otherwise               -> T.resetAnnotation Bold       >> render anns' ss
          (Italic       :anns')
            | Italic     `elem` anns' -> pure ()
            | otherwise               -> T.resetAnnotation Italic     >> render anns' ss
          (Underlined   :anns')
            | Underlined `elem` anns' -> pure ()
            | otherwise               -> T.resetAnnotation Underlined >> render anns' ss
          (Inverted     :anns')
            | Inverted   `elem` anns' -> pure ()
            | otherwise               -> T.resetAnnotation Inverted   >> render anns' ss
          (Foreground c :anns') -> case oldFG anns' of
            Just d  -> T.setAnnotation   (Foreground d) >> render anns' ss
            Nothing -> T.resetAnnotation (Foreground c) >> render anns' ss
          (Background c :anns') -> case oldBG anns' of
            Just d  -> T.setAnnotation   (Background d) >> render anns' ss
            Nothing -> T.resetAnnotation (Background c) >> render anns' ss

  setAnnotation Bold                                      = write "\ESC[1m"
  setAnnotation Italic                                    = pure ()
  setAnnotation Underlined                                = write "\ESC[4m"
  setAnnotation Inverted                                  = write "\ESC[7m"
  setAnnotation (Foreground (T.Color T.Dull   T.Black  )) = write "\ESC[30m"
  setAnnotation (Foreground (T.Color T.Dull   T.Red    )) = write "\ESC[31m"
  setAnnotation (Foreground (T.Color T.Dull   T.Green  )) = write "\ESC[32m"
  setAnnotation (Foreground (T.Color T.Dull   T.Yellow )) = write "\ESC[33m"
  setAnnotation (Foreground (T.Color T.Dull   T.Blue   )) = write "\ESC[34m"
  setAnnotation (Foreground (T.Color T.Dull   T.Magenta)) = write "\ESC[35m"
  setAnnotation (Foreground (T.Color T.Dull   T.Cyan   )) = write "\ESC[36m"
  setAnnotation (Foreground (T.Color T.Dull   T.White  )) = write "\ESC[37m"
  setAnnotation (Foreground (T.Color T.Bright T.Black  )) = write "\ESC[90m"
  setAnnotation (Foreground (T.Color T.Bright T.Red    )) = write "\ESC[91m"
  setAnnotation (Foreground (T.Color T.Bright T.Green  )) = write "\ESC[92m"
  setAnnotation (Foreground (T.Color T.Bright T.Yellow )) = write "\ESC[93m"
  setAnnotation (Foreground (T.Color T.Bright T.Blue   )) = write "\ESC[94m"
  setAnnotation (Foreground (T.Color T.Bright T.Magenta)) = write "\ESC[95m"
  setAnnotation (Foreground (T.Color T.Bright T.Cyan   )) = write "\ESC[96m"
  setAnnotation (Foreground (T.Color T.Bright T.White  )) = write "\ESC[97m"
  setAnnotation (Background (T.Color T.Dull   T.Black  )) = write "\ESC[40m"
  setAnnotation (Background (T.Color T.Dull   T.Red    )) = write "\ESC[41m"
  setAnnotation (Background (T.Color T.Dull   T.Green  )) = write "\ESC[42m"
  setAnnotation (Background (T.Color T.Dull   T.Yellow )) = write "\ESC[43m"
  setAnnotation (Background (T.Color T.Dull   T.Blue   )) = write "\ESC[44m"
  setAnnotation (Background (T.Color T.Dull   T.Magenta)) = write "\ESC[45m"
  setAnnotation (Background (T.Color T.Dull   T.Cyan   )) = write "\ESC[46m"
  setAnnotation (Background (T.Color T.Dull   T.White  )) = write "\ESC[47m"
  setAnnotation (Background (T.Color T.Bright T.Black  )) = write "\ESC[100m"
  setAnnotation (Background (T.Color T.Bright T.Red    )) = write "\ESC[101m"
  setAnnotation (Background (T.Color T.Bright T.Green  )) = write "\ESC[102m"
  setAnnotation (Background (T.Color T.Bright T.Yellow )) = write "\ESC[103m"
  setAnnotation (Background (T.Color T.Bright T.Blue   )) = write "\ESC[104m"
  setAnnotation (Background (T.Color T.Bright T.Magenta)) = write "\ESC[105m"
  setAnnotation (Background (T.Color T.Bright T.Cyan   )) = write "\ESC[106m"
  setAnnotation (Background (T.Color T.Bright T.White  )) = write "\ESC[107m"
  resetAnnotation Bold           = write "\ESC[22m"
  resetAnnotation Italic         = pure ()
  resetAnnotation Underlined     = write "\ESC[24m"
  resetAnnotation Inverted       = write "\ESC[27m"
  resetAnnotation (Foreground _) = write "\ESC[39m"
  resetAnnotation (Background _) = write "\ESC[49m"
  resetAnnotations               = write "\ESC[m"

instance (MonadIO m, MonadThrow m) => T.MonadFormatPrinter (AnsiTerminalT m) where
  bold       = Bold
  italic     = Italic
  underlined = Underlined

instance (MonadIO m, MonadThrow m) => T.MonadColorPrinter (AnsiTerminalT m) where
  inverted   = Inverted
  foreground = Foreground
  background = Background

instance (MonadIO m, MonadThrow m) => T.MonadTerminal (AnsiTerminalT m) where
  moveCursorUp i                         = write $ "\ESC[" <> Text.pack (show i) <> "A"
  moveCursorDown i                       = write $ "\ESC[" <> Text.pack (show i) <> "B"
  moveCursorForward i                    = write $ "\ESC[" <> Text.pack (show i) <> "C"
  moveCursorBackward i                   = write $ "\ESC[" <> Text.pack (show i) <> "D"
  getCursorPosition = do
    write "\ESC[6n"
    waitForCursorPositionReport
    where
      -- Swallow all incoming events until either a cursor position report
      -- arrives or an Interrupt event occurs.
      -- An interrupt event will cause an `E.UserInterrupt` exception to be thrown.
      waitForCursorPositionReport = T.waitEvent >>= \case
        T.InterruptEvent                           -> throwM E.UserInterrupt
        T.DeviceEvent (T.CursorPositionReport pos) -> pure pos
        _ -> waitForCursorPositionReport
  setCursorPosition (x,y)                = write $ "\ESC[" <> Text.pack (show x) <> ";" <> Text.pack (show y) <> "H"
  setVerticalCursorPosition i            = write $ "\ESC[" <> Text.pack (show i) <> "d"
  setHorizontalCursorPosition i          = write $ "\ESC[" <> Text.pack (show i) <> "G"
  saveCursorPosition                     = write "\ESC7"
  restoreCursorPosition                  = write "\ESC8"
  showCursor                             = write "\ESC[?25h"
  hideCursor                             = write "\ESC[?25l"

  getScreenSize = AnsiTerminalT $ do
    ansi <- ask
    liftIO $ atomically $ T.termScreenSize ansi

-- | See https://en.wikipedia.org/wiki/List_of_Unicode_characters
safeChar :: Char -> Bool
safeChar c
  | c == '\n'   = True  -- Newline
  | c == '\t'   = True  -- Horizontal tab
  | c  < '\SP'  = False -- All other C0 control characters.
  | c  < '\DEL' = True  -- Printable remainder of ASCII. Start of C1.
  | c  < '\xa0' = False -- C1 up to start of Latin-1.
  | otherwise   = True

write :: (MonadIO m) => Text.Text -> AnsiTerminalT m ()
write t = AnsiTerminalT $ do
  ansi <- ask
  liftIO $ atomically $ T.termOutput ansi t
