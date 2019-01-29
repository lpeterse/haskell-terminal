{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module System.Terminal.TerminalT
  ( TerminalT ()
  , runTerminalT
  )
where

import           Control.Applicative                ((<|>))
import           Control.Monad                      (when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Foldable                      (forM_)
import qualified Data.Text                       as Text
import qualified Data.Text.Prettyprint.Doc       as PP
import           Prelude                     hiding (putChar)
import           Control.Exception                  (AsyncException (..))

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
runTerminalT (TerminalT action) t = runReaderT action t

instance MonadTrans (TerminalT t) where
    lift = TerminalT . lift

instance (MonadIO m, T.Terminal t) => MonadInput (TerminalT t m) where
    waitInterruptOrEvent f = TerminalT do
        t <- ask
        liftIO $ atomically $ f (T.termInterrupt t) (T.termEvent t)

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadPrinter (TerminalT t m) where
    putChar c =
        command $ T.PutText $ Text.singleton c
    putString cs = forM_ (filter safeChar cs) $ \c->
        command $ T.PutText $ Text.singleton c
    putText t = loop (Text.filter safeChar t)
        where
        loop t0
            | Text.null t0 = pure ()
            | otherwise    = let (t1,t2) = Text.splitAt 80 t0
                             in  command (T.PutText t1) >> loop t2
    flush = TerminalT do
        t <- ask
        liftIO  $ T.termFlush t
    getLineWidth = TerminalT do
        t <- ask
        liftIO (snd <$> T.termGetScreenSize t)

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
    moveCursorUp                           = command . T.MoveCursorUp
    moveCursorDown                         = command . T.MoveCursorDown
    moveCursorLeft                         = command . T.MoveCursorLeft
    moveCursorRight                        = command . T.MoveCursorRight

    getCursorPosition = TerminalT (liftIO . T.termGetCursorPosition =<< ask)
    setCursorPosition                      = command . T.SetCursorPosition
    setCursorPositionVertical              = command . T.SetCursorPositionVertical
    setCursorPositionHorizontal            = command . T.SetCursorPositionHorizontal
    saveCursorPosition                     = command T.SaveCursorPosition
    restoreCursorPosition                  = command T.RestoreCursorPosition
    showCursor                             = command T.ShowCursor
    hideCursor                             = command T.HideCursor

    clearLine                              = command T.ClearLine
    clearLineLeft                          = command T.ClearLineLeft
    clearLineRight                         = command T.ClearLineRight
    clearScreen                            = command T.ClearScreen
    clearScreenAbove                       = command T.ClearScreenAbove
    clearScreenBelow                       = command T.ClearScreenBelow
    
    useAlternateScreenBuffer               = command . T.UseAlternateScreenBuffer
    
    getScreenSize = TerminalT (liftIO . T.termGetScreenSize =<< ask)

instance (MonadIO m, MonadThrow m, T.Terminal t) => MonadTerminal (TerminalT t m) where

-- | See https://en.wikipedia.org/wiki/List_of_Unicode_characters
safeChar :: Char -> Bool
safeChar c
  | c == '\n'   = True  -- Newline
  | c == '\t'   = True  -- Horizontal tab
  | c  < '\SP'  = False -- All other C0 control characters.
  | c  < '\DEL' = True  -- Printable remainder of ASCII. Start of C1.
  | c  < '\xa0' = False -- C1 up to start of Latin-1.
  | otherwise   = True

command :: (MonadIO m, MonadThrow m, T.Terminal t) => T.Command -> TerminalT t m ()
command c = TerminalT do
  t <- ask
  liftIO $ T.termCommand t c
