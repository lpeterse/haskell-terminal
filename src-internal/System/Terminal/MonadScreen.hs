module System.Terminal.MonadScreen where

import           System.Terminal.MonadInput
import           System.Terminal.MonadPrinter

class (MonadPrinter m) => MonadScreen m where
    -- | Get the dimensions of the visible window.
    --
    -- * This operation shall not cause a round-trip to the terminal and
    --   may be called as often as desired.
    -- * Deviations from reality might occur during the time when
    --   the window size was changed up to when the corresponding event
    --   updates the internal tracking state (this is unavoidable).
    getWindowSize               :: m (Rows, Cols)
    -- | Get the current cursor position as tracked internally.
    --
    -- * `(0,0) is the upper left of the window.
    -- * The cursor is always within window bounds.
    -- * This operation shall not cause a round-trip to the terminal and
    --   may be called as often as desired.
    -- * Deviations from reality might occur after window resize (use `getCursorPositionReport`
    --   to get the actual position and resync tracking state).
    getCursorPosition           :: m (Row, Col)
    -- | Get the current cursor position as reported by the terminal.
    --
    -- * `(0,0) is the upper left of the window.
    -- * The cursor is always within window bounds.
    -- * This operation causes a round-trip to the terminal and
    --   shall be used sparely (e.g. on window size change).
    -- * It automaticaly updates the internal tracking state when called.
    getCursorPositionReport     :: m (Row, Col)
    -- | Set the cursor position.
    --
    -- * `(0,0)` is the upper left of the window.
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
    --   Use `saveCursorPosition` and `loadCursorPosition` else.
    restoreCursor               :: m ()
    -- | Save cursor position (immune to scrolling, no attributes).
    saveCursorPosition          :: m ()
    -- | Load cursor position (immune to scrolling, no attributes).
    --
    -- * Returns the cursor position relative to the visible window,
    --   but takes scrolling into account. This is useful when trying
    --   to memorize a certain text position.
    -- * The resulting coordinates might be negative or greater
    --   than the window dimensions meaning that the
    --   saved position is now outside the visible window. The user
    --   is advised to check for these conditions.
    loadCursorPosition          :: m (Row, Col)

    -- | Move the cursor `n` lines up. Do not change column.
    moveCursorUp                :: Rows -> m ()
    -- | Move the cursor `n` lines down. Do not change column.
    moveCursorDown              :: Rows -> m ()
    -- | Move the cursor `n` columns to the left. Do not change line.
    moveCursorBackward          :: Cols -> m ()
    -- | Move the cursor `n` columns to the right. Do not change line.
    moveCursorForward           :: Cols -> m ()

    insertChars                 :: Int -> m ()
    deleteChars                 :: Int -> m ()
    insertLines                 :: Int -> m ()
    deleteLines                 :: Int -> m ()

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
