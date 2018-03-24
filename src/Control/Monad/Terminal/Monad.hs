module Control.Monad.Terminal.Monad where

import           Control.Monad.Terminal.Input
import           Control.Monad.Terminal.Printer

class (MonadInput m, MonadPrettyPrinter m, MonadFormatPrinter m, MonadColorPrinter m) => MonadTerminal m where
  -- | Move the cursor `n` lines up. Do not change column.
  moveCursorUp                :: Int -> m ()
  -- | Move the cursor `n` lines down. Do not change column.
  moveCursorDown              :: Int -> m ()
  -- | Move the cursor `n` columns to the left. Do not change line.
  moveCursorLeft          :: Int -> m ()
  -- | Move the cursor `n` columns to the right. Do not change line.
  moveCursorRight           :: Int -> m ()
  -- | Get the current cursor position. `(0,0) is the upper left of the screen.
  getCursorPosition           :: m (Int, Int)
  -- | Set the cursor position. `(0,0)` is the upper left of the screen.
  setCursorPosition           :: (Int,Int) -> m ()
  -- | Set the vertical cursor position to the `n`th line. Do not change column.
  setCursorPositionVertical   :: Int -> m ()
  -- | Set the horizontal cursor position to the `n`th column. Do not change line.
  setCursorPositionHorizontal :: Int -> m ()
  -- | Save the current cursor position to be restored later by `restoreCursorPosition`.
  saveCursorPosition          :: m ()
  -- | Restore cursor to position previously saved by `saveCursorPosition`.
  restoreCursorPosition       :: m ()
  -- | Show the cursor.
  showCursor                  :: m ()
  -- | Hide the cursor.
  hideCursor                  :: m ()

  -- | Clear the entire line containing the cursor.
  clearLine                   :: m ()
  -- | Clear the line from cursor left.
  clearLineLeft               :: m ()
  -- | Clear the line from cursor right.
  clearLineRight              :: m ()
  -- | Clear the entire screen.
  clearScreen                 :: m ()
  -- | Clear the screen above the cursor.
  clearScreenAbove            :: m ()
  -- | Clear the screen below the cursor.
  clearScreenBelow            :: m ()

  getScreenSize               :: m (Int,Int)
  -- | Whether or not to use the alternate screen buffer.
  --
  --   - The main screen buffer content is preserved and restored
  --     when leaving the alternate screen screen buffer.
  --   - The dimensions of the alternate screen buffer are
  --     exactly those of the screen.
  useAlternateScreenBuffer    :: Bool -> m ()

-- http://www.noah.org/python/pexpect/ANSI-X3.64.htm
-- Erasing parts of the display (EL and ED) in the VT100 is performed thus:
--
--  Erase from cursor to end of line           Esc [ 0 K    or Esc [ K
--  Erase from beginning of line to cursor     Esc [ 1 K
--  Erase line containing cursor               Esc [ 2 K
--  Erase from cursor to end of screen         Esc [ 0 J    or Esc [ J
--  Erase from beginning of screen to cursor   Esc [ 1 J
--  Erase entire screen                        Esc [ 2 J
