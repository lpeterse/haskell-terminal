{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
module System.Terminal.MonadPrinter where

import           Data.Text                     as T
import           Prelude                   hiding (putChar)

-- | This class describes an environment that Unicode text can be printed to.
--   This might either be file or a terminal.
--
--    * Instances shall implement the concept of lines and line width.
--    * Instances shall implement the concept of a carriage that can be
--      set to the beginning of the next line.
--    * It is assumed that the carriage automatically moves to the beginning
--      of the next line if the end of the current line is reached.
--    * Instances shall be Unicode aware or must at least be able to print
--      a replacement character.
--    * Implementations must be aware of infinite lazy `Prelude.String`s and
--      long `Data.Text.Text`s. `String`s should be printed character wise as
--      evaluating them might trigger exceptions at any point. Long text should
--      be printed chunk wise in order to stay interruptible.
--    * Implementations must not use an unbounded output buffer. Print operations
--      shall block and be interruptible when the output buffer is full.
--    * Instances shall not pass control characters in text to the printer (not even line break).
--      Control characters shall be replaced with �. Text formatting shall be done
--      with the designated classes extending `MonadMarkupPrinter`.
--      Allowing control sequences would cause a dependency on certain terminal
--      types, but also pose an underrated security risk as modern terminals are
--      highly programmable and should not be fed with untrusted input.
class Monad m => MonadPrinter m where
    -- | Move the carriage to the beginning of the next line.
    putLn              :: m ()
    -- | Print a single character.
    putChar            :: Char -> m ()
    -- | Print a `String`.
    putString          :: String -> m ()
    putString           = mapM_ putChar
    -- | Print a `String` and an additional newline.
    putStringLn        :: String -> m ()
    putStringLn s       = putString s >> putLn
    -- | Print a `Text`.
    putText            :: Text -> m ()
    putText             = putString . T.unpack
    -- | Print a `Text` and an additional newline.
    putTextLn          :: Text -> m ()
    putTextLn           = putStringLn . T.unpack
    -- | Flush the output buffer and make the all previous output actually
    --   visible after a reasonably short amount of time.
    --
    --    * The operation may return before the buffer has actually been flushed.
    flush              :: m ()
    flush               = pure ()
    -- | Get the current line width.
    --
    --    * The operation may return the last known line width and may not be
    --      completely precise when I/O is asynchronous.
    --    * This operations shall not block too long and rather be called more
    --      often in order to adapt to changes in line width.
    getLineWidth       :: m Int
    {-# MINIMAL putLn, putChar, putText, getLineWidth #-}

-- | This class introduces abstract constructors for text markup.
class MonadPrinter m => MonadMarkupPrinter m where
    -- | This associated type represents all possible attributes that are
    --   available in the current environment.
    --
    --   When writing polymorphic code against these monadic interfaces
    --   the concrete instantiation of this type is usually unknown and class
    --   instances are generally advised to not expose value constructors for
    --   this type.
    --
    --   Instead, subclasses like `MonadFormattingPrinter` and `MonadColorPrinter`
    --   offer abstract value constructors like `bold`, `underlined`, `inverted`
    --   which are then given meaning by the concrete class instance.
    data Attribute m
    setAttribute :: Attribute m -> m ()
    setAttribute _ = pure ()
    -- | Reset an attribute so that it does no longer affect subsequent output.
    --
    -- * Binary attributes like `bold` or `underlined` shall just be reset to their opposite.
    --
    -- * For non-binary attributes like colors all of their possible values shall be treated
    --   as equal, so that
    --
    --   @
    --   `setAttribute` (`foreground` $ `bright` `blue`) >> `resetAttribute` (`foreground` `red`)
    --   @
    --
    --   results in the foreground color attribute reset afterwards whereas after
    --
    --   @
    --   `setAttribute` (`foreground` $ `bright` `blue`) >> `resetAttribute` (`background` `red`)
    --   @
    --
    --   the foreground color is still set as `bright` `blue`.
    --
    resetAttribute  :: Attribute m -> m ()
    -- | Reset all attributes to their default.
    resetAttributes :: m ()
    -- | Shall determine wheter two attribute values would override each other
    --   or can be applied independently.
    --
    -- * Shall obey the laws of equivalence.
    resetsAttribute :: Attribute m -> Attribute m -> Bool

class MonadMarkupPrinter m => MonadFormattingPrinter m where
    -- | This attribute makes text appear __bold__.
    bold            :: Attribute m
    -- | This attribute makes text appear /italic/.
    italic          :: Attribute m
    -- | This attribute makes text appear underlined.
    underlined      :: Attribute m
    -- | This attribute swaps foreground and background (color).
    --
    --   * This operation is idempotent: Applying the attribute a second time
    --     won't swap it back. Use `resetAttribute` instead.
    inverted        :: Attribute m

-- | This class offers abstract value constructors for
--   foreground and background coloring.
class MonadMarkupPrinter m => MonadColorPrinter m where
    data Color m

    black      :: Color m
    red        :: Color m
    green      :: Color m
    yellow     :: Color m
    blue       :: Color m
    magenta    :: Color m
    cyan       :: Color m
    white      :: Color m

    bright     :: Color m -> Color m

    -- | This attribute sets the __foreground__ color (the text color).
    foreground :: Color m -> Attribute m
    -- | This attribute sets the __background__ color.
    background :: Color m -> Attribute m
