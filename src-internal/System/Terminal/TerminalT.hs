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
import           Control.Monad.Trans.State
import           Data.Foldable                      (forM_)
import qualified Data.Text                       as Text
import           Prelude                     hiding (putChar)

import           System.Terminal.MonadInput
import           System.Terminal.MonadPrinter
import           System.Terminal.MonadScreen
import           System.Terminal.MonadTerminal
import           System.Terminal.ScreenState
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
    = TerminalT (StateT ScreenState (ReaderT t m) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

-- | Run a `TerminalT` application on the given terminal.
runTerminalT :: (MonadIO m, MonadMask m, T.Terminal t) => TerminalT t m a -> t -> m a
runTerminalT tma t = runReaderT (evalStateT ma ssDefault) t
    where
        TerminalT ma = do
            size <- liftIO (T.termGetWindowSize t)
            modState (ssSetWindowSize size)
            tma

instance MonadTrans (TerminalT t) where
    lift = TerminalT . lift . lift

instance (MonadIO m, T.Terminal t) => MonadInput (TerminalT t m) where
    waitInterruptOrEvent f = TerminalT do
        t <- lift ask
        liftIO $ atomically $ f (T.termInterrupt t) (T.termEvent t)

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadPrinter (TerminalT t m) where
    putLn = do
        command T.PutLn
        modState ssPutLn
    putChar c = do
        command (T.PutText $ Text.singleton $ sanitizeChar c)
        modState (ssPutText 1)
    putString cs = do
        forM_ cs (command . T.PutText . Text.singleton . sanitizeChar)
        modState (ssPutText $ length cs)
    putText t = do
        let st = sanitizeText t
        command (T.PutText st)
        modState (ssPutText $ Text.length st)
    flush = TerminalT do
        t <- lift ask
        liftIO $ T.termFlush t
    getLineWidth = TerminalT do
        t <- lift ask
        liftIO (snd <$> T.termGetWindowSize t)

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
    getWindowSize = getState ssGetWindowSize

    moveCursorUp i
        | i > 0     = command (T.MoveCursorUp i) >> modState (ssMoveUp i)
        | i < 0     = moveCursorDown i
        | otherwise = pure ()
    moveCursorDown i
        | i > 0     = command (T.MoveCursorDown i) >> modState (ssMoveDown i)
        | i < 0     = moveCursorUp i
        | otherwise = pure ()
    moveCursorBackward i
        | i > 0     = command (T.MoveCursorLeft i) >> modState (ssMoveLeft i)
        | i < 0     = moveCursorForward i
        | otherwise = pure ()
    moveCursorForward i
        | i > 0     = command (T.MoveCursorRight i) >> modState (ssMoveRight i)
        | i < 0     = moveCursorBackward i
        | otherwise = pure ()

    getCursorPosition = do
        getState ssGetCursorPosition
    getCursorPositionReport = do
        pos <- TerminalT (liftIO . T.termGetCursorPosition =<< lift ask)
        size <- TerminalT (liftIO . T.termGetWindowSize =<< lift ask)
        modState (ssResetPositions size pos)
        pure pos
    setCursorPosition pos = do
        command (T.SetCursorPosition pos)
        modState (ssSetCursorPosition pos)

    saveCursor = do
        command T.SaveCursor
        modState ssSaveCursor
    restoreCursor = do
        command T.RestoreCursor
        modState ssRestoreCursor
    
    saveCursorPosition = do
        modState ssSaveCursorPosition
    loadCursorPosition = do
        getState ssLoadCursorPosition

    insertChars i = do
        command (T.InsertChars i)
    deleteChars i = do
        command (T.DeleteChars i)
    insertLines i = do
        command (T.InsertLines i)
    deleteLines i = do
        command (T.DeleteLines i)

    eraseInLine              = command . T.EraseInLine
    eraseInDisplay           = command . T.EraseInDisplay

    showCursor               = command T.ShowCursor
    hideCursor               = command T.HideCursor

    setAutoWrap x = do
        command (T.SetAutoWrap x)
        modState (ssSetAutoWrap x)
    setAlternateScreenBuffer x = do
        command (T.SetAlternateScreenBuffer x)
        modState (ssSetAlternateScreenBuffer x)

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadTerminal (TerminalT t m) where

-- | See https://en.wikipedia.org/wiki/List_of_Unicode_characters
safeChar :: Char -> Bool
safeChar c
  | c  < '\SP'  = False -- All other C0 control characters.
  | c  < '\DEL' = True  -- Printable remainder of ASCII. Start of C1.
  | c  < '\xa0' = False -- C1 up to start of Latin-1.
  | otherwise   = True

sanitizeChar :: Char -> Char
sanitizeChar c = if safeChar c then c else '�'

sanitizeText :: Text.Text -> Text.Text
sanitizeText t = Text.map sanitizeChar t
--    | Text.any (not . safeChar) t = Text.map sanitizeChar t
--    | otherwise                   = t

command :: (MonadIO m, MonadThrow m, T.Terminal t) => T.Command -> TerminalT t m ()
command c = TerminalT do
  t <- lift ask
  liftIO $ T.termCommand t c

modState :: Monad m => (ScreenState -> ScreenState) -> TerminalT t m ()
modState f = TerminalT (modify' f)

getState :: Monad m => (ScreenState -> a) -> TerminalT t m a
getState f = TerminalT (gets f)
