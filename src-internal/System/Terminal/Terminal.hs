module System.Terminal.Terminal where

import           Control.Monad.STM
import           Data.ByteString
import           Data.Text

import           System.Terminal.MonadInput

class Terminal t where
  -- | The terminal identification string usually extracted from the
  --   environment variable `TERM`. Should contain values like `xterm`
  --   or `rxvt-unicode`.
  termType              :: t -> ByteString
  -- | A stream of input events. The transaction will succeed as soon as the
  --   next input event becomes available.
  --
  --   Note: Trying to read more than one event within the same transaction
  --   might be successfull, but might also lead to undesired behaviour as
  --   the transaction will block until all of its preconditions are fulfilled.
  --   Some form of `orElse` needs to be used in a correct way for reading
  --   several events at once.
  termEvent             :: t -> STM Event
  -- | This transaction succeeds as soon as an interrupt occurs.
  --   Executing the transaction shall reset an interrupt flag maintained
  --   by a supervising background thread.
  --
  --   It is mandatory to regularly check this transaction in order to signal
  --   responsiveness to the background thread. The execution environment is otherwise
  --   advised to throw an `System.IO.Error.UserInterrupt` exception as soon as a
  --   second interrupt arrives and it sees a previous one unhandled.
  termInterrupt         :: t -> STM ()
  -- | This transaction appends a piece of `Data.Text.Text` to the output buffer.
  --   It shall block when the buffer exeeded its capacity
  --   and unblock as soon as space becomes available again.
  --
  --   Note: All implementations must limit the size of the output buffer or
  --   the application is at risk of running out of memory when writing much
  --   faster than the terminal can read. Using a `Control.Concurrent.STM.TMVar.TMVar`
  --   as a buffer of size 1 is perfectly fine here.
  termCommand           :: t -> Command -> IO ()
  -- | This operations flushes the output buffer. Whether it blocks or
  --   not until the buffer has actually been flushed shall be undefined
  --   (there might be other buffers involved that cannot be force-flushed
  --   so it is probably better to not give any guarantees here).
  termFlush             :: t -> IO ()
  -- | This operation shall return the latest known screen size without
  --   blocking. The first parameter denotes the number of rows and the
  --   one the number of columns.
  termGetScreenSize     :: t -> IO (Rows, Columns)
  -- | This operation shall return the current cursor position.
  --   It may block as depending on implementation it usually requires an
  --   in-band roundtrip to the terminal. Use it wisely.
  termGetCursorPosition :: t -> IO (Row, Column)

-- | The commands every terminal needs to understand.
--
-- This shall only be extended when something is missing
-- that all terminals understand. Otherwise portability will
-- be lost.
data Command
  = PutText Text
  | SetAttribute Attribute
  | ResetAttribute Attribute
  | ResetAttributes
  | MoveCursorUp Int
  | MoveCursorDown Int
  | MoveCursorLeft Int
  | MoveCursorRight Int
  | GetCursorPosition
  | SetCursorPosition (Row, Column)
  | SetCursorPositionVertical Row
  | SetCursorPositionHorizontal Column
  | SaveCursorPosition
  | RestoreCursorPosition
  | ShowCursor
  | HideCursor
  | ClearLine
  | ClearLineLeft
  | ClearLineRight
  | ClearScreen
  | ClearScreenAbove
  | ClearScreenBelow
  | UseAlternateScreenBuffer Bool
  deriving (Eq, Ord, Show)

data Attribute
  = Bold
  | Italic
  | Underlined
  | Inverted
  | Foreground Color
  | Background Color
  deriving (Eq, Ord, Show)

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
