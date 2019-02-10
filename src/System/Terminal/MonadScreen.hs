module System.Terminal.MonadScreen where

import           System.Terminal.MonadPrinter

data Size = Size
    { height :: {-# UNPACK #-} !Int
    , width  :: {-# UNPACK #-} !Int
    } deriving (Eq, Ord, Show)

data Position = Position
    { row    :: {-# UNPACK #-} !Int
    , col    :: {-# UNPACK #-} !Int
    } deriving (Eq, Ord, Show)

data EraseMode
    = EraseBackward  -- ^ Erase left of/above current cursor position.
    | EraseForward   -- ^ Erase right of/below current cursor position.
    | EraseAll       -- ^ Erase whole line/screen.
    deriving (Eq, Ord, Show)

class (MonadPrinter m) => MonadScreen m where
    -- | Get the dimensions of the visible window.
    getWindowSize               :: m Size
    -- | Move the cursor `n` lines up. Do not change column.
    moveCursorUp                :: Int -> m ()
    -- | Move the cursor `n` lines down. Do not change column.
    moveCursorDown              :: Int -> m ()
    -- | Move the cursor `n` columns to the right. Do not change line.
    moveCursorForward           :: Int -> m ()
    -- | Move the cursor `n` columns to the left. Do not change line.
    moveCursorBackward          :: Int -> m ()
    -- | Get the current cursor position as reported by the terminal.
    --
    -- * @Position 0 0@ is the upper left of the window.
    -- * The cursor is always within window bounds.
    -- * This operation causes a round-trip to the terminal and
    --   shall be used sparely (e.g. on window size change).
    getCursorPosition           :: m Position
    -- | Set the cursor position.
    --
    -- * @Position 0 0@ is the upper left of the window.
    -- * The resulting cursor position is undefined when it is outside
    --   the window bounds.
    setCursorPosition           :: Position -> m ()
    -- | Set the cursor row.
    --
    -- * @0@ is the topmost row.
    setCursorRow                :: Int -> m ()
    -- | Set the cursor column.
    --
    -- * @0@ is the leftmost column.
    setCursorColumn             :: Int -> m ()
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
    -- | Insert whitespace at the cursor position and
    --   shift existing characters to the right.
    insertChars                 :: Int -> m ()
    -- | Delete characters and shift existing characters from the right.
    deleteChars                 :: Int -> m ()
    -- | Replace characters with whitespace.
    eraseChars                  :: Int -> m ()
    -- | Insert lines and shift existing lines downwards.
    insertLines                 :: Int -> m ()
    -- | Delete lines and shift up existing lines from below.
    deleteLines                 :: Int -> m ()
    -- | Clears characters in the current line.
    eraseInLine                 :: EraseMode -> m ()
    -- | Clears lines above/below the current line.
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
