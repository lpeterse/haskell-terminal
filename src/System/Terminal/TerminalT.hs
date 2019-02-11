{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module System.Terminal.TerminalT
  ( TerminalT ()
  , runTerminalT
  )
where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Foldable                      (forM_)
import qualified Data.Text                       as Text
import           Prelude                     hiding (putChar)

import           System.Terminal.MonadInput
import           System.Terminal.MonadPrinter
import           System.Terminal.MonadScreen
import           System.Terminal.MonadTerminal
import qualified System.Terminal.Terminal        as T

-- | This monad transformer represents terminal applications.
--
-- It implements all classes in this module and should serve as a good
-- foundation for most use cases.
--
-- Note that it is not necessary nor recommended to have this type in
-- every signature. Keep your application abstract and mention `TerminalT`
-- only once at the top level.
--
-- Example:
--
-- @
-- main :: IO ()
-- main = `withTerminal` (`runTerminalT` myApplication)
--
-- myApplication :: (`MonadPrinter` m) => m ()
-- myApplication = do
--     `putTextLn` "Hello world!"
--     `flush`
-- @
newtype TerminalT t m a
    = TerminalT (ReaderT t m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

-- | Run a `TerminalT` application on the given terminal.
runTerminalT :: (MonadIO m, MonadMask m, T.Terminal t) => TerminalT t m a -> t -> m a
runTerminalT tma t = runReaderT ma t
    where
        TerminalT ma = tma

instance MonadTrans (TerminalT t) where
    lift = TerminalT . lift

instance (MonadIO m, T.Terminal t) => MonadInput (TerminalT t m) where
    awaitWith f = TerminalT do
        t <- ask
        liftIO $ atomically $ f (T.termInterrupt t) (T.termEvent t)
    setBracketedPasteMode b =
        command (T.SetBracketedPasteMode b)

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadPrinter (TerminalT t m) where
    putLn =
        command T.PutLn
    putChar c =
        command (T.PutText $ Text.singleton c)
    putString cs =
        forM_ cs (command . T.PutText . Text.singleton)
    putText t = 
        command (T.PutText t)
    flush = TerminalT do
        t <- ask
        liftIO $ T.termFlush t
    getLineWidth = TerminalT do
        t <- ask
        liftIO (width <$> T.termGetWindowSize t)

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadMarkupPrinter (TerminalT t m) where
    data Attribute (TerminalT t m) = AttributeT T.Attribute deriving (Eq, Ord, Show)
    setAttribute   (AttributeT a)  = command (T.SetAttribute   a)
    resetAttribute (AttributeT a)  = command (T.ResetAttribute a)
    resetAttributes                = command T.ResetAttributes
    resetsAttribute (AttributeT T.Bold       {}) (AttributeT T.Bold       {}) = True
    resetsAttribute (AttributeT T.Italic     {}) (AttributeT T.Italic     {}) = True
    resetsAttribute (AttributeT T.Underlined {}) (AttributeT T.Underlined {}) = True
    resetsAttribute (AttributeT T.Inverted   {}) (AttributeT T.Inverted   {}) = True
    resetsAttribute (AttributeT T.Foreground {}) (AttributeT T.Foreground {}) = True
    resetsAttribute (AttributeT T.Foreground {}) (AttributeT T.Background {}) = True
    resetsAttribute _                            _                            = False 

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadFormattingPrinter (TerminalT t m) where
    bold       = AttributeT T.Bold
    italic     = AttributeT T.Italic
    underlined = AttributeT T.Underlined
    inverted   = AttributeT T.Inverted

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadColorPrinter (TerminalT t m) where
    data Color (TerminalT t m) = ColorT T.Color deriving (Eq, Ord, Show)
    black                      = ColorT T.Black
    red                        = ColorT T.Red
    green                      = ColorT T.Green
    yellow                     = ColorT T.Yellow
    blue                       = ColorT T.Blue
    magenta                    = ColorT T.Magenta
    cyan                       = ColorT T.Cyan
    white                      = ColorT T.White
    bright (ColorT T.Black  )  = ColorT T.BrightBlack
    bright (ColorT T.Red    )  = ColorT T.BrightRed
    bright (ColorT T.Green  )  = ColorT T.BrightGreen
    bright (ColorT T.Yellow )  = ColorT T.BrightYellow
    bright (ColorT T.Blue   )  = ColorT T.BrightBlue
    bright (ColorT T.Magenta)  = ColorT T.BrightMagenta
    bright (ColorT T.Cyan   )  = ColorT T.BrightCyan
    bright (ColorT T.White  )  = ColorT T.BrightWhite
    bright (ColorT c        )  = ColorT c
    foreground (ColorT c)      = AttributeT (T.Foreground c)
    background (ColorT c)      = AttributeT (T.Background c)

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadScreen (TerminalT t m) where
    getWindowSize =
        TerminalT (liftIO . T.termGetWindowSize =<< ask)

    moveCursorUp i
        | i > 0     = command (T.MoveCursorUp i)
        | i < 0     = moveCursorDown i
        | otherwise = pure ()
    moveCursorDown i
        | i > 0     = command (T.MoveCursorDown i)
        | i < 0     = moveCursorUp i
        | otherwise = pure ()
    moveCursorForward i
        | i > 0     = command (T.MoveCursorForward i)
        | i < 0     = moveCursorBackward i
        | otherwise = pure ()
    moveCursorBackward i
        | i > 0     = command (T.MoveCursorBackward i)
        | i < 0     = moveCursorForward i
        | otherwise = pure ()

    getCursorPosition =
        TerminalT (liftIO . T.termGetCursorPosition =<< ask)
    setCursorPosition pos =
        command (T.SetCursorPosition pos)
    setCursorRow i =
        command (T.SetCursorRow i)
    setCursorColumn i =
        command (T.SetCursorColumn i)

    saveCursor =
        command T.SaveCursor
    restoreCursor =
        command T.RestoreCursor

    insertChars i = do
        command (T.InsertChars i)
    deleteChars i = do
        command (T.DeleteChars i)
    eraseChars  i = do
        command (T.EraseChars  i)
    insertLines i = do
        command (T.InsertLines i)
    deleteLines i = do
        command (T.DeleteLines i)

    eraseInLine =
        command . T.EraseInLine
    eraseInDisplay =
        command . T.EraseInDisplay

    showCursor =
        command T.ShowCursor
    hideCursor =
        command T.HideCursor

    setAutoWrap x =
        command (T.SetAutoWrap x)
    setAlternateScreenBuffer x =
        command (T.SetAlternateScreenBuffer x)

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadTerminal (TerminalT t m) where

command :: (MonadIO m, T.Terminal t) => T.Command -> TerminalT t m ()
command c = TerminalT do
  t <- ask
  liftIO $ T.termCommand t c
