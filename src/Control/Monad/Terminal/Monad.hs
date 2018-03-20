module Control.Monad.Terminal.Monad where

import           Control.Monad.Terminal.Input
import           Control.Monad.Terminal.Printer

class (MonadInput m, MonadPrettyPrinter m, MonadFormatPrinter m, MonadColorPrinter m) => MonadTerminal m where
  -- | Moves the cursor up `n` lines. Does not change column.
  moveCursorUp                :: Int -> m ()
  -- | Moves the cursor down `n` lines. Does not change column.
  moveCursorDown              :: Int -> m ()
  -- | Moves the cursor forward `n` columns. Does not change line.
  moveCursorForward           :: Int -> m ()
  -- | Moves the cursor backward `n` columns. Does not change line.
  moveCursorBackward          :: Int -> m ()
  -- | Gets the current cursor position. `(0,0) is the upper left of the screen.
  getCursorPosition           :: m (Int, Int)
  -- | Sets the cursor position. `(0,0)` is the upper left of the screen.
  setCursorPosition           :: (Int,Int) -> m ()
  -- | Sets the vertical cursor position to the `n`th line. Does not change column.
  setVerticalCursorPosition   :: Int -> m ()
  -- | Sets the horizontal cursor position to the `n`th column. Does not change line.
  setHorizontalCursorPosition :: Int -> m ()
  -- | Saves the current cursor position to be restored later by `restoreCursorPosition`.
  saveCursorPosition          :: m ()
  -- | Restore cursor to position previously saved by `saveCursorPosition`.
  restoreCursorPosition       :: m ()
  -- | Shows the cursor.
  showCursor                  :: m ()
  -- | Hides the cursor.
  hideCursor                  :: m ()
  getScreenSize               :: m (Int,Int)
  clearLine                   :: m ()
  useAlternateScreenBuffer    :: Bool -> m ()