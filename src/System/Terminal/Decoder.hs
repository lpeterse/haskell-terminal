module System.Terminal.Decoder where

import           Data.Char
import           Data.Monoid                  ((<>))

import           System.Terminal.MonadInput
import           System.Terminal.MonadScreen (Position (..))

-- | The type `Decoder` is a finite state transducer.
--
--   Intermediate state can be passed as closure.
--   See below for an example.
newtype Decoder = Decoder { feedDecoder :: Modifiers -> Char -> (Decoder, [Event]) }

defaultDecoder :: (Modifiers -> Char -> Maybe Event) -> Decoder
defaultDecoder specialChar = defaultMode
  where
    emit :: [Event] -> (Decoder, [Event])
    emit evs = (defaultMode, evs)

    illegal :: (Decoder, [Event])
    illegal = (defaultMode, [OtherEvent "illegal sequence"])

    -- The default mode is the decoder's entry point.
    defaultMode :: Decoder
    defaultMode = Decoder $ \mods c -> if
        -- In normal mode a NUL is interpreted as a fill character and skipped.
        | c == '\NUL' -> emit []
        -- ESC might or might not introduce an escape sequence.
        | c == '\ESC' -> (escapeMode, [])
        -- All other C0 control codes are mapped to their corresponding ASCII character + CTRL modifier.
        -- If the character is a special character, then two events are produced.
        | c <= '\US'  -> emit $ maybe [KeyEvent (CharKey (toEnum $ (+64) $ fromEnum c)) (mods <> ctrlKey)] pure (specialChar mods c)
        -- All remaning characters of the Latin-1 block are returned as is.
        | c <  '\DEL' -> emit $ [KeyEvent (CharKey c) mods] ++ f mods c
        -- Skip all other C1 control codes and DEL unless they have special meaning configured.
        | c <  '\xA0' -> emit $ f mods c
        -- All other Unicode characters are returned as is.
        | otherwise   -> emit [KeyEvent (CharKey c) mods]
        where
            f mods c = maybe [] pure (specialChar mods c)

    pasteMode :: Decoder
    pasteMode = awaitEsc
      where
        awaitEsc, awaitBracket, await2, await0, await1, awaitTilde :: Decoder
        awaitEsc = Decoder $ const \case
          '\ESC' -> (awaitBracket, [])
          c      -> (awaitEsc, f "" c)
        awaitBracket = Decoder $ const \case
          '['    -> (await2, [])
          c      -> (awaitEsc, f "\ESC" c)
        await2 = Decoder $ const \case
          '2'    -> (await0, [])
          c      -> (awaitEsc, f "\ESC[" c)
        await0 = Decoder $ const \case
          '0'    -> (await1, [])
          c      -> (awaitEsc, f "\ESC[2" c)
        await1 = Decoder $ const \case
          '1'    -> (awaitTilde, [])
          c      -> (awaitEsc, f "\ESC[20" c)
        awaitTilde = Decoder $ const \case
          '~'    -> (defaultMode, [PasteEvent PasteEnd])
          c      -> (awaitEsc, f "\ESC[201" c)
        f :: String -> Char -> [Event]
        f s c = fmap (PasteEvent . Pasted) (s ++ [c])

    -- This function shall be called if an ESC has been read in default mode
    -- and it is stil unclear whether this is the beginning of an escape sequence or not.
    -- NOTE: This function is total and consumes at least one more character of input.
    escapeMode :: Decoder
    escapeMode  = Decoder $ \mods c-> if
      -- Single escape key press is always followed by a NUL fill character
      -- by design (instead of timing). This makes reasoning and testing much easier
      -- and reliable.
      | c == '\NUL' -> emit [KeyEvent (CharKey '[') (mods <> ctrlKey), KeyEvent EscapeKey mods]
      | otherwise   -> (escapeSequenceMode c, [])

    -- This function shall be called with the escape sequence introducer.
    -- It needs to look at next character to decide whether this is
    -- a CSI sequence or an ALT-modified key or illegal state.
    escapeSequenceMode :: Char -> Decoder
    escapeSequenceMode c = Decoder $ \mods d-> if
      | d == '\NUL' && c > '\SP' && c <= '~' -> emit [KeyEvent (CharKey c) (mods <> altKey)]
      | d == '\NUL' && c >= '\xa0'           -> emit [KeyEvent (CharKey c) (mods <> altKey)]
      | d == '\NUL'                          -> case specialChar mods c of
                                                  Nothing -> unknown d
                                                  Just ev -> case ev of
                                                    KeyEvent key m -> emit [KeyEvent key (mods <> m <> altKey)]
                                                    _              -> emit [ev]
      | c == 'O'                             -> ss3Mode mods d
      | c == '['                             -> csiMode d
      | otherwise                            -> unknown d
          where
            unknown d = emit [OtherEvent $ "unknown sequence " ++ show ('\ESC':c:[d])]

    -- SS3 mode is another less well-known escape sequence mode.
    -- It is introduced by `\\ESCO`. Some terminal emulators use it for
    -- compatibility with veeery old terminals. SS3 mode only allows one
    -- subsequent character. Interpretation has been determined empirically
    -- and with reference to http://rtfm.etla.org/xterm/ctlseq.html
    ss3Mode :: Modifiers -> Char -> (Decoder, [Event])
    ss3Mode mods = \case
      'P' -> emit [KeyEvent (FunctionKey  1) mods]
      'Q' -> emit [KeyEvent (FunctionKey  2) mods]
      'R' -> emit [KeyEvent (FunctionKey  3) mods]
      'S' -> emit [KeyEvent (FunctionKey  4) mods]
      d   -> emit [OtherEvent $ "unknown sequence " ++ show ("\ESCO" ++ [d])]

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
    csiMode :: Char -> (Decoder, [Event])
    csiMode c
      | c >= '0' && c <= '?' = f (charLimit - 1) [c]
      | c >= '!' && c <= '/' = g (charLimit - 1) [] [c]
      | c >= '@' && c <= '~' = interpretCSI [] [] c
      | otherwise            = illegal
      where
        charLimit :: Int
        charLimit  = 16
        -- Note: The following functions use recursion, but recursion is
        -- guaranteed to terminate and maximum recursion depth is only
        -- dependant on the constant `charLimit`. In case of errors the decoder
        -- will therefore recover to default mode after at most 32 characters.
        f :: Int -> String -> (Decoder, [Event])
        f 0 _  = illegal
        f i ps = (,[]) $ Decoder $ const $ \x-> if
          | x >= '0' && x <= '?' -> f (i - 1) (x:ps)  -- More parameters.
          | x >= '!' && x <= '/' -> g charLimit ps [] -- Start of intermediates.
          | x >= '@' && x <= '~' -> interpretCSI (reverse ps) [] x
          | otherwise            -> illegal
        g :: Int -> String -> String -> (Decoder, [Event])
        g 0 _  _  = illegal
        g i ps is = (, []) $ Decoder $ const $ \x-> if
          | x >= '!' && x <= '/' -> g (i - 1) ps (x:is) -- More intermediates.
          | x >= '@' && x <= '~' -> interpretCSI (reverse ps) (reverse is) x
          | otherwise            -> illegal

    interpretCSI :: String -> String -> Char -> (Decoder, [Event])
    interpretCSI params intermediates d = case d of
      '$'        -> emit [KeyEvent DeleteKey (altKey `mappend` shiftKey)]  -- urxvt, gnome-terminal
      '@'        -> unknown
      'A'        -> modified $ ArrowKey Upwards
      'B'        -> modified $ ArrowKey Downwards
      'C'        -> modified $ ArrowKey Rightwards
      'D'        -> modified $ ArrowKey Leftwards
      'E'        -> modified   BeginKey
      'F'        -> modified   EndKey
      'G'        -> unknown
      'H'        -> modified   HomeKey
      'I'        -> modified   TabKey
      'J'        -> unknown
      'K'        -> unknown
      'L'        -> unknown
      'M'        -> unknown
      'N'        -> unknown
      'O'        -> unknown
      'P'        -> modified (FunctionKey  1)
      'Q'        -> modified (FunctionKey  2)
      -- This sequence is ambiguous. xterm and derivatives use this to encode a modified F3 key as
      -- well as a cursor position report. There is no real solution to disambiguate these two
      -- other than context of expectation (cursor position report has probably been requested).
      -- This decoder shall simply emit both events and the user shall ignore unexpected events.
      'R'        -> let (dec', evs) = modified (FunctionKey  3)
                    in (dec', evs ++ [DeviceEvent $ CursorPositionReport $ Position (fstNumber 1 - 1) (sndNumber 1 - 1)])
      'S'        -> modified (FunctionKey  4)
      'T'        -> unknown
      'U'        -> unknown
      'V'        -> unknown
      'W'        -> unknown
      'X'        -> unknown
      'Y'        -> unknown
      'Z'        -> emit [KeyEvent TabKey shiftKey]
      '^'        -> case params of
        "2"  -> emit [KeyEvent InsertKey        ctrlKey]
        "3"  -> emit [KeyEvent DeleteKey        ctrlKey]
        "4"  -> emit [KeyEvent PageUpKey        ctrlKey]
        "7"  -> emit [KeyEvent PageDownKey      ctrlKey]
        "5"  -> emit [KeyEvent HomeKey          ctrlKey]
        "6"  -> emit [KeyEvent EndKey           ctrlKey]
        "11" -> emit [KeyEvent (FunctionKey  1) ctrlKey]
        "12" -> emit [KeyEvent (FunctionKey  2) ctrlKey]
        "13" -> emit [KeyEvent (FunctionKey  3) ctrlKey]
        "14" -> emit [KeyEvent (FunctionKey  4) ctrlKey]
        "15" -> emit [KeyEvent (FunctionKey  5) ctrlKey]
        "17" -> emit [KeyEvent (FunctionKey  6) ctrlKey]
        "18" -> emit [KeyEvent (FunctionKey  7) ctrlKey]
        "19" -> emit [KeyEvent (FunctionKey  8) ctrlKey]
        "20" -> emit [KeyEvent (FunctionKey  9) ctrlKey]
        "21" -> emit [KeyEvent (FunctionKey 10) ctrlKey]
        "23" -> emit [KeyEvent (FunctionKey 11) ctrlKey]
        "24" -> emit [KeyEvent (FunctionKey 12) ctrlKey]
        _    -> unknown
      'f' -> unknown
      'i' -> emit [KeyEvent PrintKey mempty]
      'm' -> unknown
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
        "200" -> (pasteMode, [PasteEvent PasteBegin])
        "201" -> emit [PasteEvent PasteEnd]
        _ -> unknown
      _ -> unknown
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
          ""  -> emit [KeyEvent key   mempty                       ]
          "2" -> emit [KeyEvent key   shiftKey                     ]
          "3" -> emit [KeyEvent key               altKey           ]
          "4" -> emit [KeyEvent key $ shiftKey <> altKey           ]
          "5" -> emit [KeyEvent key                         ctrlKey]
          "6" -> emit [KeyEvent key $ shiftKey <>           ctrlKey]
          "7" -> emit [KeyEvent key $             altKey <> ctrlKey]
          "8" -> emit [KeyEvent key $ shiftKey <> altKey <> ctrlKey]
          _   -> unknown
        unknown :: (Decoder, [Event])
        unknown = emit [OtherEvent $ "unknown sequence " ++ show
          ("\ESC[" ++ params ++ intermediates ++ [d])]