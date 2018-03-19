{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Control.Monad.Terminal.Decoder where

import           Data.Char
import           Data.Monoid                  ((<>))

import           Control.Monad.Terminal.Input

-- | The type `Decoder` represents a finite state transducer.
--
--   Values of this type can only be constructed by tying the knot
--   which causes the resulting transducer to have one entry point
--   but no exits. Intermediate state can be passed as closures.
--   See below for an example.
newtype Decoder = Decoder { feedDecoder :: Modifiers -> Char -> ([Event], Decoder) }

ansiDecoder :: (Char -> Maybe Event) -> Decoder
ansiDecoder specialChars = defaultMode
  where
    -- Make a production and return to `defaultMode`.
    produce   :: [Event] -> ([Event], Decoder)
    produce es = (es, defaultMode)
    -- Continue processing with the given decoder.
    -- Shall be used for state/mode change.
    continue  :: Decoder -> ([Event], Decoder)
    continue d = ([], d)

    -- The default mode is the decoder's entry point and is returned to
    -- after each successful sequence decoding or as fail safe mode after
    -- occurences of illegal state (this decoder shall skip errors and will
    -- probably resynchronise on the input stream after a few characters).
    defaultMode :: Decoder
    defaultMode  = Decoder $ \mods c-> if
        -- In normal mode a NUL is interpreted as a fill character and skipped.
        | c == '\NUL' -> produce []
        -- ESC might or might not introduce an escape sequence.
        | c == '\ESC' -> continue escapeMode
        -- All other C0 control codes are mapped to their corresponding ASCII character + CTRL modifier.
        -- If the character is a special character, then two events are produced.
        | c <= '\US'  -> produce $ KeyEvent (CharKey (toEnum $ (+64) $ fromEnum c)) (mods <> ctrlKey) : specialKey mods c
        -- Space shall be interpreted as control code and translated to `SpaceKey` to be
        -- consistent with the handling of `TabKey` and other whitespaces.
        | c == '\SP'  -> produce [KeyEvent SpaceKey mods]
        -- All remaning characters of the Latin-1 block are returned as is.
        | c <  '\DEL' -> produce [KeyEvent (CharKey c) mods]
        -- DEL is a very delicate case. It might be `KeyBackspace` or `KeyDelete`.
        | c == '\DEL' -> produce $ KeyEvent (CharKey '?') (mods <> ctrlKey) : specialKey mods c
        -- Skip all other C1 control codes excpect if they are special characters.
        | c <  '\xA0' -> produce $ specialKey mods c
        -- All other Unicode characters are returned as is.
        | otherwise   -> produce $ KeyEvent (CharKey c) mods : specialKey mods c
        where
          specialKey mods c = case specialChars c of
            Nothing -> []
            Just ev -> case ev of
              KeyEvent key m -> [KeyEvent key (mods <> m)]
              _              -> [ev]

    -- This function shall be called if an ESC has been read in default mode
    -- and it is stil unclear whether this is the beginning of an escape sequence or not.
    -- NOTE: This function is total and consumes at least one more character of input.
    escapeMode :: Decoder
    escapeMode  = Decoder $ \mods c-> if
      -- Single escape key press is always followed by a NUL fill character
      -- by design (instead of timing). This makes reasoning and testing much easier
      -- and reliable.
      | c == '\NUL' -> produce [KeyEvent (CharKey '[') (mods <> ctrlKey), KeyEvent EscapeKey mods]
      | otherwise   -> continue (escapeSequenceMode c)

    -- This function shall be called with the escape sequence introducer.
    -- It needs to look at next character to decide whether this is
    -- a CSI sequence or an ALT-modified key or illegal state.
    escapeSequenceMode :: Char -> Decoder
    escapeSequenceMode c = Decoder $ \mods d-> if
      | d == '\NUL' && c > '\SP' && c <= '~' -> produce [KeyEvent (CharKey c) (mods <> altKey)]
      | d == '\NUL' && c >= '\xa0'           -> produce [KeyEvent (CharKey c) (mods <> altKey)]
      | d == '\NUL'                          -> produce $ case specialChars c of
                                                  Nothing -> []
                                                  Just ev -> case ev of
                                                    KeyEvent key m -> [KeyEvent key (mods <> m <> altKey)]
                                                    _              -> [ev]
      | c == 'O'                             -> produce (ss3Mode mods d)
      | c == '['                             -> csiMode d
      | otherwise                            -> produce []

    -- SS3 mode is another less well-known escape sequence mode.
    -- It is introduced by `\\ESCO`. Some terminal emulators use it for
    -- compatibility with veeery old terminals. SS3 mode only allows one
    -- subsequent character. Interpretation has been determined empirically
    -- and with reference to http://rtfm.etla.org/xterm/ctlseq.html
    ss3Mode :: Modifiers -> Char -> [Event]
    ss3Mode mods = \case
      'P' -> [KeyEvent (FunctionKey  1) mods]
      'Q' -> [KeyEvent (FunctionKey  2) mods]
      'R' -> [KeyEvent (FunctionKey  3) mods]
      'S' -> [KeyEvent (FunctionKey  4) mods]
      _   -> []

    -- ESC[ is followed by any number (including none) of parameter chars in the
    -- range 0–9:;<=>?, then by any number of intermediate chars
    -- in the range space and !"#$%&'()*+,-./, then finally by a single char in
    -- the range @A–Z[\]^_`a–z{|}~.
    -- For security reasons (untrusted input and denial of service) this parser
    -- only accepts a very limited number of characters for both parameter and
    -- intermediate chars.
    -- Unknown (not illegal) sequences are dropped, but it is guaranteed that
    -- they will be consumed completely and it is safe for the parser to
    -- return to normal mode afterwards. Illegal sequences cause the parser
    -- to consume the input up to the first violating character and then reject.
    -- The parser might be out of sync afterwards, but this is a protocol
    -- violation anyway. The parser's only job here is not to loop (consume
    -- and drop the illegal input!) and then to stop and fail reliably.
    csiMode :: Char -> ([Event], Decoder)
    csiMode c
      | c >= '0' && c <= '?' = continue $ f (charLimit - 1) [c]
      | c >= '!' && c <= '/' = continue $ g (charLimit - 1) [] [c]
      | c >= '@' && c <= '~' = produce $ interpretCSI [] [] c
      | otherwise            = produce [] -- Illegal state. Return to default mode.
      where
        charLimit :: Int
        charLimit  = 16
        -- Note: The following functions use recursion, but recursion is
        -- guaranteed to terminate and maximum recursion depth is only
        -- dependant on the constant `charLimit`. In case of errors the decoder
        -- will therefore recover to default mode after at most 32 characters.
        f :: Int -> String -> Decoder
        f 0 _  = defaultMode
        f i ps = Decoder $ const $ \c-> if
          | c >= '0' && c <= '?' -> continue $ f (i - 1) (c:ps)  -- More parameters.
          | c >= '!' && c <= '/' -> continue $ g charLimit ps [] -- Start of intermediates.
          | c >= '@' && c <= '~' -> produce $ interpretCSI (reverse ps) [] c
          | otherwise            -> produce [] -- Illegal state. Return to default mode.
        g :: Int -> String -> String -> Decoder
        g 0 _  _  = defaultMode
        g i ps is = Decoder $ const $ \c-> if
          | c >= '!' && c <= '/' -> continue $ g (i - 1) ps (c:is) -- More intermediates.
          | c >= '@' && c <= '~' -> produce $ interpretCSI (reverse ps) (reverse is) c
          | otherwise            -> produce [] -- Illegal state. Return to default mode.

interpretCSI :: String -> String -> Char -> [Event]
interpretCSI params intermediates = \case
  '$'        -> [KeyEvent DeleteKey (altKey `mappend` shiftKey)]  -- urxvt, gnome-terminal
  '@'        -> []
  'A'        -> modified $ ArrowKey Upwards
  'B'        -> modified $ ArrowKey Downwards
  'C'        -> modified $ ArrowKey Rightwards
  'D'        -> modified $ ArrowKey Leftwards
  'E'        -> modified   BeginKey
  'F'        -> modified   EndKey
  'G'        -> []
  'H'        -> modified   HomeKey
  'I'        -> modified   TabKey
  'J'        -> []
  'K'        -> []
  'L'        -> []
  'M'        -> []
  'N'        -> []
  'O'        -> []
  'P'        -> modified (FunctionKey  1)
  'Q'        -> modified (FunctionKey  2)
  -- This sequence is ambiguous. xterm and derivatives use this to encode a modified F3 key as
  -- well as a cursor position report. There is no real solution to disambiguate these two
  -- other than context of expectation (cursor position report has probably been requested).
  -- This decoder shall simply emit both events and the user shall ignore unexpected events.
  'R'        -> modified (FunctionKey  3) ++ [DeviceEvent $ CursorPositionReport (fstNumber 0, sndNumber 0)]
  'S'        -> modified (FunctionKey  4)
  'T'        -> []
  'U'        -> []
  'V'        -> []
  'W'        -> []
  'X'        -> []
  'Y'        -> []
  'Z'        -> [KeyEvent TabKey shiftKey]
  '^'        -> case params of
    "2"  -> [KeyEvent InsertKey        ctrlKey]
    "3"  -> [KeyEvent DeleteKey        ctrlKey]
    "4"  -> [KeyEvent PageUpKey        ctrlKey]
    "7"  -> [KeyEvent PageDownKey      ctrlKey]
    "5"  -> [KeyEvent HomeKey          ctrlKey]
    "6"  -> [KeyEvent EndKey           ctrlKey]
    "11" -> [KeyEvent (FunctionKey  1) ctrlKey]
    "12" -> [KeyEvent (FunctionKey  2) ctrlKey]
    "13" -> [KeyEvent (FunctionKey  3) ctrlKey]
    "14" -> [KeyEvent (FunctionKey  4) ctrlKey]
    "15" -> [KeyEvent (FunctionKey  5) ctrlKey]
    "17" -> [KeyEvent (FunctionKey  6) ctrlKey]
    "18" -> [KeyEvent (FunctionKey  7) ctrlKey]
    "19" -> [KeyEvent (FunctionKey  8) ctrlKey]
    "20" -> [KeyEvent (FunctionKey  9) ctrlKey]
    "21" -> [KeyEvent (FunctionKey 10) ctrlKey]
    "23" -> [KeyEvent (FunctionKey 11) ctrlKey]
    "24" -> [KeyEvent (FunctionKey 12) ctrlKey]
    _    -> []
  'f' -> []
  'i' -> [KeyEvent PrintKey mempty]
  'm' -> []
  '~' -> case fstParam of
    "2"  -> modified InsertKey
    "3"  -> modified DeleteKey
    "5"  -> modified PageUpKey
    "6"  -> modified PageDownKey
    "9"  -> modified HomeKey
    "10" -> modified EndKey
    "11" -> modified (FunctionKey 1)
    "12" -> modified (FunctionKey 2)
    "13" -> modified (FunctionKey 3)
    "14" -> modified (FunctionKey 4)
    "15" -> modified (FunctionKey 5)
    "17" -> modified (FunctionKey 6)
    "18" -> modified (FunctionKey 7)
    "19" -> modified (FunctionKey 8)
    "20" -> modified (FunctionKey 9)
    "21" -> modified (FunctionKey 10)
    "23" -> modified (FunctionKey 11)
    "24" -> modified (FunctionKey 12)
    _    -> []
  _ -> []
  where
    fstParam :: String
    fstParam = takeWhile (/= ';') params
    sndParam :: String
    sndParam = takeWhile (/= ';') $ drop 1 $ dropWhile (/= ';') params
    fstNumber :: Int -> Int
    fstNumber i
      | not (null fstParam) && all isDigit fstParam = read fstParam
      | otherwise                                   = i
    sndNumber :: Int -> Int
    sndNumber i
      | not (null sndParam) && all isDigit sndParam = read sndParam
      | otherwise                                   = i
    modified key = case sndParam of
      ""  -> [KeyEvent key   mempty                       ]
      "2" -> [KeyEvent key   shiftKey                     ]
      "3" -> [KeyEvent key               altKey           ]
      "4" -> [KeyEvent key $ shiftKey <> altKey           ]
      "5" -> [KeyEvent key                         ctrlKey]
      "6" -> [KeyEvent key $ shiftKey <>           ctrlKey]
      "7" -> [KeyEvent key $             altKey <> ctrlKey]
      "8" -> [KeyEvent key $ shiftKey <> altKey <> ctrlKey]
      _   -> []
