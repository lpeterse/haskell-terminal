module System.Terminal.MonadScreen where

import           System.Terminal.MonadPrinter

type Row  = Int
type Rows = Int
type Col  = Int
type Cols = Int

data EraseMode
    = EraseBackward
    | EraseForward
    | EraseAll
    deriving (Eq, Ord, Show)

class (MonadPrinter m) => MonadScreen m where
    -- | Get the dimensions of the visible window.
    getWindowSize               :: m (Rows, Cols)

    -- | Move the cursor `n` lines up. Do not change column.
    moveCursorUp                :: Rows -> m ()
    -- | Move the cursor `n` lines down. Do not change column.
    moveCursorDown              :: Rows -> m ()
    -- | Move the cursor `n` columns to the right. Do not change line.
    moveCursorForward           :: Cols -> m ()
    -- | Move the cursor `n` columns to the left. Do not change line.
    moveCursorBackward          :: Cols -> m ()

    -- | Get the current cursor position as reported by the terminal.
    --
    -- * (0,0) is the upper left of the window.
    -- * The cursor is always within window bounds.
    -- * This operation causes a round-trip to the terminal and
    --   shall be used sparely (e.g. on window size change).
    getCursorPosition           :: m (Row, Col)
    -- | Set the cursor position.
    --
    -- * (0,0) is the upper left of the window.
    -- * The resulting cursor position is undefined when it is outside
    --   the window bounds.
    setCursorPosition           :: (Row, Col) -> m ()

    -- | Save cursor position and attributes.
    saveCursor                  :: m ()
    -- | Restore cursor position and attributes.
    --
    -- * Restores the cursor as previously saved by `saveCursor`.
    -- * The cursor position is strictly relative to the visible
    --   window and does not take eventual scrolling into account.
    --   The advantage of this operation is that it does not require
    --   transmission of coordinates and attributes to the terminal
    --   and is therefor slightly more efficient than all other alternatives.
    -- * Only use this when auto-wrap is disabled, alternate screen
    --   buffer is enabled or you can otherwise guarantee that the
    --   window does not scroll between `saveCursor` and `restoreCursor`!
    restoreCursor               :: m ()

    insertChars                 :: Int -> m ()
    deleteChars                 :: Int -> m ()
    eraseChars                  :: Int -> m ()
    insertLines                 :: Int -> m ()
    deleteLines                 :: Int -> m ()

    eraseInLine                 :: EraseMode -> m ()
    eraseInDisplay              :: EraseMode -> m ()

    -- | Show the cursor.
    showCursor                  :: m ()
    -- | Hide the cursor.
    hideCursor                  :: m ()

    -- | Whether or not to automatically wrap on line ends.
    setAutoWrap                 :: Bool -> m ()
    -- | Whether or not to use the alternate screen buffer.
    --
    --   - The main screen buffer content is preserved and restored
    --     when leaving the alternate screen screen buffer.
    --   - The dimensions of the alternate screen buffer are
    --     exactly those of the screen.
    setAlternateScreenBuffer    :: Bool -> m ()
