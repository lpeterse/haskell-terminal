module System.Terminal.Encoder where

import qualified Data.Text as T

import           System.Terminal.Terminal
import           System.Terminal.MonadScreen

-- | See https://en.wikipedia.org/wiki/List_of_Unicode_characters
safeChar :: Char -> Bool
safeChar c
  | c  < '\SP'  = False -- All other C0 control characters.
  | c  < '\DEL' = True  -- Printable remainder of ASCII. Start of C1.
  | c  < '\xa0' = False -- C1 up to start of Latin-1.
  | otherwise   = True
{-# INLINE safeChar #-}

sanitizeChar :: Char -> Char
sanitizeChar c = if safeChar c then c else '�'
{-# INLINE sanitizeChar #-}

sanitizeText :: T.Text -> T.Text
sanitizeText t
    | T.any (not . safeChar) t = T.map sanitizeChar t
    | otherwise                = t
{-# INLINE sanitizeText #-}

defaultEncode :: Command -> T.Text
defaultEncode = \case
    PutLn                                    -> "\n"
    PutText t                                -> sanitizeText t
    SetAttribute Bold                        -> "\ESC[1m"
    SetAttribute Italic                      -> ""
    SetAttribute Underlined                  -> "\ESC[4m"
    SetAttribute Inverted                    -> "\ESC[7m"
    SetAttribute (Foreground       Black  )  -> "\ESC[30m"
    SetAttribute (Foreground       Red    )  -> "\ESC[31m"
    SetAttribute (Foreground       Green  )  -> "\ESC[32m"
    SetAttribute (Foreground       Yellow )  -> "\ESC[33m"
    SetAttribute (Foreground       Blue   )  -> "\ESC[34m"
    SetAttribute (Foreground       Magenta)  -> "\ESC[35m"
    SetAttribute (Foreground       Cyan   )  -> "\ESC[36m"
    SetAttribute (Foreground       White  )  -> "\ESC[37m"
    SetAttribute (Foreground BrightBlack  )  -> "\ESC[90m"
    SetAttribute (Foreground BrightRed    )  -> "\ESC[91m"
    SetAttribute (Foreground BrightGreen  )  -> "\ESC[92m"
    SetAttribute (Foreground BrightYellow )  -> "\ESC[93m"
    SetAttribute (Foreground BrightBlue   )  -> "\ESC[94m"
    SetAttribute (Foreground BrightMagenta)  -> "\ESC[95m"
    SetAttribute (Foreground BrightCyan   )  -> "\ESC[96m"
    SetAttribute (Foreground BrightWhite  )  -> "\ESC[97m"
    SetAttribute (Background       Black  )  -> "\ESC[40m"
    SetAttribute (Background       Red    )  -> "\ESC[41m"
    SetAttribute (Background       Green  )  -> "\ESC[42m"
    SetAttribute (Background       Yellow )  -> "\ESC[43m"
    SetAttribute (Background       Blue   )  -> "\ESC[44m"
    SetAttribute (Background       Magenta)  -> "\ESC[45m"
    SetAttribute (Background       Cyan   )  -> "\ESC[46m"
    SetAttribute (Background       White  )  -> "\ESC[47m"
    SetAttribute (Background BrightBlack  )  -> "\ESC[100m"
    SetAttribute (Background BrightRed    )  -> "\ESC[101m"
    SetAttribute (Background BrightGreen  )  -> "\ESC[102m"
    SetAttribute (Background BrightYellow )  -> "\ESC[103m"
    SetAttribute (Background BrightBlue   )  -> "\ESC[104m"
    SetAttribute (Background BrightMagenta)  -> "\ESC[105m"
    SetAttribute (Background BrightCyan   )  -> "\ESC[106m"
    SetAttribute (Background BrightWhite  )  -> "\ESC[107m"
    ResetAttribute Bold                      -> "\ESC[22m"
    ResetAttribute Italic                    -> ""
    ResetAttribute Underlined                -> "\ESC[24m"
    ResetAttribute Inverted                  -> "\ESC[27m"
    ResetAttribute (Foreground _)            -> "\ESC[39m"
    ResetAttribute (Background _)            -> "\ESC[49m"
    ResetAttributes                          -> "\ESC[m"
    ShowCursor                               -> "\ESC[?25h"
    HideCursor                               -> "\ESC[?25l"
    SaveCursor                               -> "\ESC7"
    RestoreCursor                            -> "\ESC8"
    MoveCursorUp          i | i <= 0         -> ""
                            | i == 0         -> "\ESC[A"
                            | otherwise      -> "\ESC[" <> T.pack (show i) <> "A"
    MoveCursorDown        i | i <= 0         -> ""
                            | i == 0         -> "\ESC[B"
                            | otherwise      -> "\ESC[" <> T.pack (show i) <> "B"
    MoveCursorForward     i | i <= 0         -> ""
                            | i == 0         -> "\ESC[C"
                            | otherwise      -> "\ESC[" <> T.pack (show i) <> "C"
    MoveCursorBackward    i | i <= 0         -> ""
                            | i == 0         -> "\ESC[D"
                            | otherwise      -> "\ESC[" <> T.pack (show i) <> "D"
    GetCursorPosition                        -> "\ESC[6n"
    SetCursorPosition (x,y)                  -> "\ESC[" <> T.pack (show $ x + 1) <> ";" <> T.pack (show $ y + 1) <> "H"
    SetCursorVertical i                      -> "\ESC[" <> T.pack (show $ i + 1) <> "d"
    SetCursorHorizontal i                    -> "\ESC[" <> T.pack (show $ i + 1) <> "G"
    InsertChars i   | i <= 0                 -> ""
                    | i == 1                 -> "\ESC[@"
                    | otherwise              -> "\ESC[" <> T.pack (show i) <> "@"
    DeleteChars i   | i <= 0                 -> ""
                    | i == 1                 -> "\ESC[P"
                    | otherwise              -> "\ESC[" <> T.pack (show i) <> "P"
    EraseChars  i   | i <= 0                 -> ""
                    | i == 1                 -> "\ESC[X"
                    | otherwise              -> "\ESC[" <> T.pack (show i) <> "X"
    InsertLines i   | i <= 0                 -> ""
                    | i == 1                 -> "\ESC[L"
                    | otherwise              -> "\ESC[" <> T.pack (show i) <> "L"
    DeleteLines i   | i <= 0                 -> ""
                    | i == 1                 -> "\ESC[M"
                    | otherwise              -> "\ESC[" <> T.pack (show i) <> "M"
    EraseInLine     EraseForward             -> "\ESC[0K"
    EraseInLine     EraseBackward            -> "\ESC[1K"
    EraseInLine     EraseAll                 -> "\ESC[2K"
    EraseInDisplay  EraseForward             -> "\ESC[0J"
    EraseInDisplay  EraseBackward            -> "\ESC[1J"
    EraseInDisplay  EraseAll                 -> "\ESC[2J"
    SetAutoWrap True                         -> "\ESC[?7h"
    SetAutoWrap False                        -> "\ESC[?7l"
    SetAlternateScreenBuffer True            -> "\ESC[?1049h"
    SetAlternateScreenBuffer False           -> "\ESC[?1049l"

-- http://www.noah.org/python/pexpect/ANSI-X3.64.htm
-- Erasing parts of the display (EL and ED) in the VT100 is performed thus:
--
--  Erase from cursor to end of line           Esc [ 0 K    or Esc [ K
--  Erase from beginning of line to cursor     Esc [ 1 K
--  Erase line containing cursor               Esc [ 2 K
--  Erase from cursor to end of screen         Esc [ 0 J    or Esc [ J
--  Erase from beginning of screen to cursor   Esc [ 1 J
--  Erase entire screen                        Esc [ 2 J
