module System.Terminal.Modes where

data Modes
  = VINTR          (Maybe Char) -- Interrupt character; 255 if none.
  | VQUIT          (Maybe Char) -- The quit character (sends SIGQUIT signal on POSIX systems).
  | VERASE         (Maybe Char) -- Erase the character to left of the cursor.
  | VKILL          (Maybe Char) -- Kill the current input line.
  | VEOF           (Maybe Char) -- End-of-file character (sends EOF from the terminal).
  | VEOL           (Maybe Char) -- End-of-line character in addition to carriage return and/or linefeed.
  | VEOL2          (Maybe Char) -- Additional end-of-line character.
  | VSTART         (Maybe Char) -- Continues paused output (normally control-Q).
  | VSTOP          (Maybe Char) -- Pauses output (normally control-S).
  | VSUSP          (Maybe Char) -- Suspends the current program.
  | VDSUSP         (Maybe Char) -- Another suspend character.
  | VREPRINT       (Maybe Char) -- Reprints the current input line.
  | VWERASE        (Maybe Char) -- Erases a word left of cursor.
  | VLNEXT         (Maybe Char) -- Enter the next character typed literally, even if it is a special character
  | VFLUSH         (Maybe Char) -- Character to flush output.
  | VSWTCH         (Maybe Char) -- Switch to a different shell layer.
  | VSTATUS        (Maybe Char) -- Prints system status line (load, command, pid, etc).
  | VDISCARD       (Maybe Char) -- Toggles the flushing of terminal output.
  | IGNPAR         (Maybe Bool) -- The ignore parity flag.  The parameter SHOULD be 0 if this flag is FALSE, and 1 if it is TRUE.
  | PARMRK         (Maybe Bool) -- Mark parity and framing errors.
  | INPCK          (Maybe Bool) -- Enable checking of parity errors.
  | ISTRIP         (Maybe Bool) -- Strip 8th bit off characters.
  | INLCR          (Maybe Bool) -- Map NL into CR on input.
  | IGNCR          (Maybe Bool) -- Ignore CR on input.
  | ICRNL          (Maybe Bool) -- Map CR to NL on input.
  | IUCLC          (Maybe Bool) -- Translate uppercase characters to lowercase.
  | IXON           (Maybe Bool) -- Enable output flow control.
  | IXANY          (Maybe Bool) -- Any char will restart after stop.
  | IXOFF          (Maybe Bool) -- Enable input flow control.
  | IMAXBEL        (Maybe Bool) -- Ring bell on input queue full.
  | IUTF8          (Maybe Bool) -- Terminal is UTF8 capable.
  | ISIG           (Maybe Bool) -- Enable signals INTR, QUIT, [D]SUSP.
  | ICANON         (Maybe Bool) -- Canonicalize input lines.
  | XCASE          (Maybe Bool) -- Enable input and output of uppercase characters by preceding their lowercase equivalents with "\".
  | ECHO           (Maybe Bool) -- Enable echoing.
  | ECHOE          (Maybe Bool) -- Visually erase chars.
  | ECHOK          (Maybe Bool) -- Kill character discards current line.
  | ECHONL         (Maybe Bool) -- Echo NL even if ECHO is off.
  | NOFLSH         (Maybe Bool) -- Don't flush after interrupt.
  | TOSTOP         (Maybe Bool) -- Stop background jobs from output.
  | IEXTEN         (Maybe Bool) -- Enable extensions.
  | ECHOCTL        (Maybe Bool) -- Echo control characters as ^(Char).
  | ECHOKE         (Maybe Bool) -- Visual erase for line kill.
  | PENDIN         (Maybe Bool) -- Retype pending input.
  | OPOST          (Maybe Bool) -- Enable output processing.
  | OLCUC          (Maybe Bool) -- Convert lowercase to uppercase.
  | ONLCR          (Maybe Bool) -- Map NL to CR-NL.
  | OCRNL          (Maybe Bool) -- Translate carriage return to newline (output).
  | ONOCR          (Maybe Bool) -- Translate newline to carriage return-newline (output).
  | ONLRET         (Maybe Bool) -- Newline performs a carriage return (output).
  | CS7            (Maybe Bool) -- 7 bit mode.
  | CS8            (Maybe Bool) -- 8 bit mode.
  | PARENB         (Maybe Bool) -- Parity enable.
  | PARODD         (Maybe Bool) -- Odd parity, else even.
  | TTY_OP_ISPEED  (Maybe Int)  -- Specifies the input baud rate in bits per second.
  | TTY_OP_OSPEED  (Maybe Int)  -- Specifies the output baud rate in bits per second.
