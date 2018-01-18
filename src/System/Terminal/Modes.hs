module System.Terminal.Modes where

data TermModes
  = TermModes
  { modeVINTR       :: Maybe Char --   1 Interrupt character; 255 if none.
  , modeVQUIT       :: Maybe Char --   2 The quit character (sends SIGQUIT signal on POSIX systems).
  , modeVERASE      :: Maybe Char --   3 Erase the character to left of the cursor.
  , modeVKILL       :: Maybe Char --   4 Kill the current input line.
  , modeVEOF        :: Maybe Char --   5 End-of-file character (sends EOF from the terminal).
  , modeVEOL        :: Maybe Char --   6 End-of-line character in addition to carriage return and/or linefeed.
  , modeVEOL2       :: Maybe Char --   7 Additional end-of-line character.
  , modeVSTART      :: Maybe Char --   8 Continues paused output (normally control-Q).
  , modeVSTOP       :: Maybe Char --   9 Pauses output (normally control-S).
  , modeVSUSP       :: Maybe Char --  10 Suspends the current program.
  , modeVDSUSP      :: Maybe Char --  11 Another suspend character.
  , modeVREPRINT    :: Maybe Char --  12 Reprints the current input line.
  , modeVWERASE     :: Maybe Char --  13 Erases a word left of cursor.
  , modeVLNEXT      :: Maybe Char --  14 Enter the next character typed literally, even if it is a special character
  , modeVFLUSH      :: Maybe Char --  15 Character to flush output.
  , modeVSWTCH      :: Maybe Char --  16 Switch to a different shell layer.
  , modeVSTATUS     :: Maybe Char --  17 Prints system status line (load, command, pid, etc).
  , modeVDISCARD    :: Maybe Char --  18 Toggles the flushing of terminal output.
  , modeIGNPAR      :: Maybe Bool --  30 The ignore parity flag.  The parameter SHOULD be 0 if this flag is FALSE, and 1 if it is TRUE.
  , modePARMRK      :: Maybe Bool --  31 Mark parity and framing errors.
  , modeINPCK       :: Maybe Bool --  32 Enable checking of parity errors.
  , modeISTRIP      :: Maybe Bool --  33 Strip 8th bit off characters.
  , modeINLCR       :: Maybe Bool --  34 Map NL into CR on input.
  , modeIGNCR       :: Maybe Bool --  35 Ignore CR on input.
  , modeICRNL       :: Maybe Bool --  36 Map CR to NL on input.
  , modeIUCLC       :: Maybe Bool --  37 Translate uppercase characters to lowercase.
  , modeIXON        :: Maybe Bool --  38 Enable output flow control.
  , modeIXANY       :: Maybe Bool --  39 Any char will restart after stop.
  , modeIXOFF       :: Maybe Bool --  40 Enable input flow control.
  , modeIMAXBEL     :: Maybe Bool --  41 Ring bell on input queue full.
  , modeIUTF8       :: Maybe Bool --  42 Terminal is UTF8 capable.
  , modeISIG        :: Maybe Bool --  50 Enable signals INTR, QUIT, [D]SUSP.
  , modeICANON      :: Maybe Bool --  51 Canonicalize input lines.
  , modeXCASE       :: Maybe Bool --  52 Enable input and output of uppercase characters by preceding their lowercase equivalents with "\".
  , modeECHO        :: Maybe Bool --  53 Enable echoing.
  , modeECHOE       :: Maybe Bool --  54 Visually erase chars.
  , modeECHOK       :: Maybe Bool --  55 Kill character discards current line.
  , modeECHONL      :: Maybe Bool --  56 Echo NL even if ECHO is off.
  , modeNOFLSH      :: Maybe Bool --  57 Don't flush after interrupt.
  , modeTOSTOP      :: Maybe Bool --  58 Stop background jobs from output.
  , modeIEXTEN      :: Maybe Bool --  59 Enable extensions.
  , modeECHOCTL     :: Maybe Bool --  60 Echo control characters as ^(Char).
  , modeECHOKE      :: Maybe Bool --  61 Visual erase for line kill.
  , modePENDIN      :: Maybe Bool --  62 Retype pending input.
  , modeOPOST       :: Maybe Bool --  70 Enable output processing.
  , modeOLCUC       :: Maybe Bool --  71 Convert lowercase to uppercase.
  , modeONLCR       :: Maybe Bool --  72 Map NL to CR-NL.
  , modeOCRNL       :: Maybe Bool --  73 Translate carriage return to newline (output).
  , modeONOCR       :: Maybe Bool --  74 Translate newline to carriage return-newline (output).
  , modeONLRET      :: Maybe Bool --  75 Newline performs a carriage return (output).
  , modeCS7         :: Maybe Bool --  90 7 bit mode.
  , modeCS8         :: Maybe Bool --  91 8 bit mode.
  , modePARENB      :: Maybe Bool --  92 Parity enable.
  , modePARODD      :: Maybe Bool --  93 Odd parity, else even.
  , modeTTYOPISPEED :: Maybe  Int -- 128 Specifies the input baud rate in bits per second.
  , modeTTYOPOSPEED :: Maybe  Int -- 129 Specifies the output baud rate in bits per second.
  } deriving (Eq, Ord, Show)

instance Monoid TermModes where
  mempty = TermModes
    { modeVINTR       = Nothing
    , modeVQUIT       = Nothing
    , modeVERASE      = Nothing
    , modeVKILL       = Nothing
    , modeVEOF        = Nothing
    , modeVEOL        = Nothing
    , modeVEOL2       = Nothing
    , modeVSTART      = Nothing
    , modeVSTOP       = Nothing
    , modeVSUSP       = Nothing
    , modeVDSUSP      = Nothing
    , modeVREPRINT    = Nothing
    , modeVWERASE     = Nothing
    , modeVLNEXT      = Nothing
    , modeVFLUSH      = Nothing
    , modeVSWTCH      = Nothing
    , modeVSTATUS     = Nothing
    , modeVDISCARD    = Nothing
    , modeIGNPAR      = Nothing
    , modePARMRK      = Nothing
    , modeINPCK       = Nothing
    , modeISTRIP      = Nothing
    , modeINLCR       = Nothing
    , modeIGNCR       = Nothing
    , modeICRNL       = Nothing
    , modeIUCLC       = Nothing
    , modeIXON        = Nothing
    , modeIXANY       = Nothing
    , modeIXOFF       = Nothing
    , modeIMAXBEL     = Nothing
    , modeIUTF8       = Nothing
    , modeISIG        = Nothing
    , modeICANON      = Nothing
    , modeXCASE       = Nothing
    , modeECHO        = Nothing
    , modeECHOE       = Nothing
    , modeECHOK       = Nothing
    , modeECHONL      = Nothing
    , modeNOFLSH      = Nothing
    , modeTOSTOP      = Nothing
    , modeIEXTEN      = Nothing
    , modeECHOCTL     = Nothing
    , modeECHOKE      = Nothing
    , modePENDIN      = Nothing
    , modeOPOST       = Nothing
    , modeOLCUC       = Nothing
    , modeONLCR       = Nothing
    , modeOCRNL       = Nothing
    , modeONOCR       = Nothing
    , modeONLRET      = Nothing
    , modeCS7         = Nothing
    , modeCS8         = Nothing
    , modePARENB      = Nothing
    , modePARODD      = Nothing
    , modeTTYOPISPEED = Nothing
    , modeTTYOPOSPEED = Nothing
    }
  mappend x y = TermModes
    { modeVINTR       = f (modeVINTR       x) (modeVINTR       y)
    , modeVQUIT       = f (modeVQUIT       x) (modeVQUIT       y)
    , modeVERASE      = f (modeVERASE      x) (modeVERASE      y)
    , modeVKILL       = f (modeVKILL       x) (modeVKILL       y)
    , modeVEOF        = f (modeVEOF        x) (modeVEOF        y)
    , modeVEOL        = f (modeVEOL        x) (modeVEOL        y)
    , modeVEOL2       = f (modeVEOL2       x) (modeVEOL2       y)
    , modeVSTART      = f (modeVSTART      x) (modeVSTART      y)
    , modeVSTOP       = f (modeVSTOP       x) (modeVSTOP       y)
    , modeVSUSP       = f (modeVSUSP       x) (modeVSUSP       y)
    , modeVDSUSP      = f (modeVDSUSP      x) (modeVDSUSP      y)
    , modeVREPRINT    = f (modeVREPRINT    x) (modeVREPRINT    y)
    , modeVWERASE     = f (modeVWERASE     x) (modeVWERASE     y)
    , modeVLNEXT      = f (modeVLNEXT      x) (modeVLNEXT      y)
    , modeVFLUSH      = f (modeVFLUSH      x) (modeVFLUSH      y)
    , modeVSWTCH      = f (modeVSWTCH      x) (modeVSWTCH      y)
    , modeVSTATUS     = f (modeVSTATUS     x) (modeVSTATUS     y)
    , modeVDISCARD    = f (modeVDISCARD    x) (modeVDISCARD    y)
    , modeIGNPAR      = f (modeIGNPAR      x) (modeIGNPAR      y)
    , modePARMRK      = f (modePARMRK      x) (modePARMRK      y)
    , modeINPCK       = f (modeINPCK       x) (modeINPCK       y)
    , modeISTRIP      = f (modeISTRIP      x) (modeISTRIP      y)
    , modeINLCR       = f (modeINLCR       x) (modeINLCR       y)
    , modeIGNCR       = f (modeIGNCR       x) (modeIGNCR       y)
    , modeICRNL       = f (modeICRNL       x) (modeICRNL       y)
    , modeIUCLC       = f (modeIUCLC       x) (modeIUCLC       y)
    , modeIXON        = f (modeIXON        x) (modeIXON        y)
    , modeIXANY       = f (modeIXANY       x) (modeIXANY       y)
    , modeIXOFF       = f (modeIXOFF       x) (modeIXOFF       y)
    , modeIMAXBEL     = f (modeIMAXBEL     x) (modeIMAXBEL     y)
    , modeIUTF8       = f (modeIUTF8       x) (modeIUTF8       y)
    , modeISIG        = f (modeISIG        x) (modeISIG        y)
    , modeICANON      = f (modeICANON      x) (modeICANON      y)
    , modeXCASE       = f (modeXCASE       x) (modeXCASE       y)
    , modeECHO        = f (modeECHO        x) (modeECHO        y)
    , modeECHOE       = f (modeECHOE       x) (modeECHOE       y)
    , modeECHOK       = f (modeECHOK       x) (modeECHOK       y)
    , modeECHONL      = f (modeECHONL      x) (modeECHONL      y)
    , modeNOFLSH      = f (modeNOFLSH      x) (modeNOFLSH      y)
    , modeTOSTOP      = f (modeTOSTOP      x) (modeTOSTOP      y)
    , modeIEXTEN      = f (modeIEXTEN      x) (modeIEXTEN      y)
    , modeECHOCTL     = f (modeECHOCTL     x) (modeECHOCTL     y)
    , modeECHOKE      = f (modeECHOKE      x) (modeECHOKE      y)
    , modePENDIN      = f (modePENDIN      x) (modePENDIN      y)
    , modeOPOST       = f (modeOPOST       x) (modeOPOST       y)
    , modeOLCUC       = f (modeOLCUC       x) (modeOLCUC       y)
    , modeONLCR       = f (modeONLCR       x) (modeONLCR       y)
    , modeOCRNL       = f (modeOCRNL       x) (modeOCRNL       y)
    , modeONOCR       = f (modeONOCR       x) (modeONOCR       y)
    , modeONLRET      = f (modeONLRET      x) (modeONLRET      y)
    , modeCS7         = f (modeCS7         x) (modeCS7         y)
    , modeCS8         = f (modeCS8         x) (modeCS8         y)
    , modePARENB      = f (modePARENB      x) (modePARENB      y)
    , modePARODD      = f (modePARODD      x) (modePARODD      y)
    , modeTTYOPISPEED = f (modeTTYOPISPEED x) (modeTTYOPISPEED y)
    , modeTTYOPOSPEED = f (modeTTYOPOSPEED x) (modeTTYOPOSPEED y)
    }
    where
      f Nothing  Nothing  = Nothing
      f (Just x) Nothing  = Just x
      f _        (Just y) = Just y

termModes :: String -> TermModes
termModes "xterm"                 = termModesXterm
termModes "xterm-256color"        = termModesXterm256Color
termModes "rxvt-unicode-256color" = termModesRxvtUnicode
termModes _                       = mempty

termModesDefault :: TermModes
termModesDefault = mempty
  { modeVINTR  = Just '\ETX'
  , modeVQUIT  = Just '\FS'
  }

termModesXterm :: TermModes
termModesXterm = termModesDefault
  { modeVERASE = Just '\b'
  }

termModesXterm256Color :: TermModes
termModesXterm256Color = termModesXterm

termModesRxvtUnicode :: TermModes
termModesRxvtUnicode = termModesDefault
  { modeVERASE = Just '\DEL'
  }
