module System.Terminal.Terminal where

import           Control.Monad.STM
import           Data.ByteString
import           Data.Text

import           System.Terminal.MonadInput
import           System.Terminal.MonadScreen (Size (..), Position (..), EraseMode (..))

-- | Types that represent terminals need to implement this class in order
--   to be driven by this library.
--
-- This library ships with two instances:
--
-- * `System.Terminal.Platform.LocalTerminal` represents the local
--   terminal wired to the process.
-- * `System.Terminal.Virtual.VirtualTerminal` is a minimal in-process
--   terminal emulator designed to be used for unit-testing terminal applications. 
class Terminal t where
  -- | The terminal identification string usually extracted from the
  --   environment variable @TERM@. Should contain values like @xterm@
  --   or @rxvt-unicode@.
  termType              :: t -> ByteString
  -- | A stream of input events. The transaction will succeed as soon as the
  --   next input event becomes available.
  --
  --   Note: Trying to read more than one event within the same transaction
  --   might be successfull, but might also lead to undesired behaviour as
  --   the transaction will block until all of its preconditions are fulfilled.
  termEvent             :: t -> STM Event
  -- | This transaction succeeds as soon as an interrupt occurs.
  --   Executing the transaction shall reset an interrupt flag maintained
  --   by a supervising background thread.
  --
  --   It is mandatory to regularly check this transaction in order to signal
  --   responsiveness to the background thread. The execution environment is otherwise
  --   advised to throw an `System.IO.Error.UserInterrupt` exception as soon as a
  --   second interrupt arrives and it sees a previous one unhandled.
  termInterrupt         :: t -> STM Interrupt
  -- | This operation shall send a command to the terminal.
  --   It shall block when the buffer exeeded its capacity
  --   and unblock as soon as space becomes available again.
  --
  --   Note: All implementations must limit the size of the output buffer or
  --   the application is at risk of running out of memory when writing much
  --   faster than the terminal can read.
  termCommand           :: t -> Command -> IO ()
  -- | This operations flushes the output buffer. Whether it blocks or
  --   not until the buffer has actually been flushed shall be undefined
  --   (there might be other buffers involved that cannot be force-flushed
  --   so it is probably better to not give any guarantees here).
  termFlush             :: t -> IO ()
  -- | This operation shall return the latest known window size without
  --   blocking.
  termGetWindowSize     :: t -> IO Size
  -- | This operation shall return the current cursor position.
  --   It may block as depending on implementation it usually requires an
  --   in-band roundtrip to the terminal. Use it wisely.
  termGetCursorPosition :: t -> IO Position

-- | The commands every terminal needs to understand.
--
-- This shall only be extended when something is missing
-- that all terminals understand. Otherwise portability will
-- be lost.
data Command
  = PutLn
  | PutText                  Text
  | SetAttribute             Attribute
  | ResetAttribute           Attribute
  | ResetAttributes
  | MoveCursorUp             Int
  | MoveCursorDown           Int
  | MoveCursorForward        Int
  | MoveCursorBackward       Int
  | ShowCursor
  | HideCursor
  | SaveCursor
  | RestoreCursor
  | GetCursorPosition
  | SetCursorPosition        Position
  | SetCursorVertical        Int
  | SetCursorHorizontal      Int
  | InsertChars              Int
  | DeleteChars              Int
  | EraseChars               Int
  | InsertLines              Int
  | DeleteLines              Int
  | EraseInLine              EraseMode
  | EraseInDisplay           EraseMode
  | SetAutoWrap              Bool
  | SetAlternateScreenBuffer Bool
  deriving (Eq, Ord, Show)

-- | ANSI text attributes.
data Attribute
  = Bold
  | Italic
  | Underlined
  | Inverted
  | Foreground Color
  | Background Color
  deriving (Eq, Ord, Show)

-- | ANSI colors.
data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | BrightBlack
  | BrightRed
  | BrightGreen
  | BrightYellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | BrightWhite
  deriving (Eq, Ord, Show)
