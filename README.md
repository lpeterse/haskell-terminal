terminal [![Hackage](https://img.shields.io/github/release/lpeterse/haskell-terminal.svg)](https://github.com/lpeterse/haskell-terminal/releases) [![Travis](https://img.shields.io/travis/lpeterse/haskell-terminal.svg)](https://travis-ci.org/lpeterse/haskell-terminal)
=======================

_terminal_ is a driver library for ANSI terminals like _xterm_.

## Features

  - Abstract monadic interfaces for different concerns: Write code that is only allowed to print
    to the screen using the `MonadColorPrinter m => m ()` constraint!
  - A monad transformer `TerminalT` which implements all of the interfaces.
    Either use it directly or include it in your monad transformer stack and lift/derive
    the functions you need.
  - Unicode support by design (assuming all terminals understand UTF-8; Windows support is implemented separately). 
  - Supports the `Text` instead of `String` movement without being to radical about it.
  - Windows support:
      - Windows 10 finally supports ANSI escape sequences and the _Windows Console_ now essentially
        behaves like an _xterm_.
      - Windows 8, Windows 7 and older is not supported. Windows 7's support officially ended in 2015 and
        the extended support will end in 2020. As this is a hobby project aiming at
        enthusiasts, I have no intention to bloat this code base with all the quirks necessary
        to make it work on older versions of Windows.
      - Unicode is fully supported on Windows for input and output and independant of unreliably
        hacks like changing the code page. A Unicode compatible console font needs to be configured.
  - A very small set of dependencies, most of which are likely to be included
    in every Haskell project anyway.
  - No dependency on terminfo (see below).
  - Rich event handling (partly inspired by _vty_):
      - Keyboard events (all control codes and escape sequences are mapped to a useful set of keys and modifiers).
      - Mouse events (TODO on Linux).
      - Screen resize events.
      - Window focus events.
      - Interrupt events.
      - Event handling is implemented using [STM](https://hackage.haskell.org/package/stm) instead of `IO`
        which makes it very easy to wait for several events simultaneously or combine it
        with custom or external events like timeouts.        
  - Proper signal handling (Ctrl+C):
      - When using the standard terminal, the library will hook the
        interrupt signal (or something equivalent e.g. on Windows).
        Incoming interrupts are passed to the application code and can be
        dispatched and processed. A supervisor thread assures that the application
        gets killed on a second interrupt when the application is non-responsive.
        This resembles the default behavior of GHC's RTS and a lot of work has been
        invested to make this mechanism work reliably.
  - Integrates the relatively new [prettyprinter](https://hackage.haskell.org/package/prettyprinter)
    library. Nicely formatted and colorful output requires nothing more than a few combinators.

## To use or not to use _terminfo_

The [terminfo](https://hackage.haskell.org/package/terminfo) library is a binding to
[libtinfo](https://en.wikipedia.org/wiki/Terminfo).

_libtinfo_ is a library that queries a database (usually below `/usr/share/terminfo`)
to determine the specifica and necessary control codes for interacting with a given
terminal.

Unfortunately, it is a reocurring source of issues:

- https://github.com/commercialhaskell/stack/issues/1012
- https://github.com/purescript/purescript/issues/2176
- https://ghc.haskell.org/trac/ghc/ticket/8746?cversion=0&cnum_hist=2
- https://ghc.haskell.org/trac/ghc/ticket/13210

Arguments in favor of _terminfo_:

  - Would allow to support all terminals in existence.
  - It's "the standard".

Arguments against _terminfo_:

  - Static linking and stand-alone binaries:
    Apart from eventual linking issues, _terminfo_ has a runtime dependency on the
    terminfo database. This might be an issue when the environment is restricted
    (`chroot` environment or if the process shall not be allowed to interact with the file
    system for security reasons).
  - _terminfo_ offers more than 500 capabilities. Only a very small part of it
    is actually needed and since there is no legacy code to support there is no
    real reason to expose more than a small subset of capabilities that is supported
    by all terminals (-> ANSI sequences).
  - Claim: All relevant terminals support and understand the relevant ANSI escape sequences
    and/or try to behave like _xterm_. Terminals that don't are not relevant.
  - Is it really necessary to support something like _tvi925_ (Televideo 925, around 1982)?
    I honor that _terminfo_ takes the burden to maintain the definition files
    for such historical hardware, but I doubt that anyone would miss it if we decide not
    to support it.

For now, I decided to not use _terminfo_ and see how well it works.
This decision might be revised in the future. The API won't be affected by it. 

## How _terminal_ compares to..

### ansi-terminal

  - [ansi-terminal](https://hackage.haskell.org/package/ansi-terminal)
    offers very similar primitives for printing to the terminal
    and controlling the cursor.
  - It also achieves doing this in portable way (very good Windows support,
    no _terminfo_ requirement on Linux/Posix).
  - It doesn't offer mechanisms for event processing.
  - Its operations live in `IO` (control code output is possible as well)
    and assume that the terminal is either connected to `stdin/stdout` or
    to a handle.

### ansi-wl-pprint

  - [ansi-wl-pprint](https://hackage.haskell.org/package/ansi-wl-pprint) is an
    extension library to _ansi-terminal_. It offers a Wadler-Leyen pretty-printer
    adapted to the needs of terminal screens (colors and text formatting).
  - _terminal_ has a dependency on the more generic
    [prettyprinter](https://hackage.haskell.org/package/prettyprinter) in order
    to offer the same features and make pretty and colorful terminal output
    the default rather than an exception.
 
### Haskeline

  - [haskeline](https://hackage.haskell.org/package/haskeline) is a pure-Haskell
    [readline](https://en.wikipedia.org/wiki/GNU_Readline) replacement.
  - Its primary job is offering a line editing interface and it does this very well.
  - Like _terminal_ it offers a monad transformer interface to the user (`InputT`).
  - It does signal handling (Ctrl+C, Ctrl+D).
  - It has a dependency on _terminfo_ in order to support a broad range of terminals
    (especially those that are non-ANSI).
  - It offers operations for printing to the terminal which pass control codes
    unescaped.
  - It might be interesting to investigate whether _terminal_ could be used
    as an alternative backend for _haskeline_.

### vty

  - [vty](https://hackage.haskell.org/package/vty) is a library that serves
    as a foundation for _curses_-like applications (full-screen terminal applications
    like `vim` or `htop`).
  - It is very similar to `terminal` (especially the event processing has been inspired
    by _vty_): It completely abstracts away the details and quirks of
    communication with different terminals and offers a canonical interface to the user.
  - Its scope is wider than that of _terminal_:
    - _vty_ has the concept of `Images` that can be assembled and manipulated by the user.
      The library keeps track of the changes and computes minimal changesets which it
      then transmits to the terminal.
  - Compared to _terminal_ it (currently) has the following shortcomings:
    - Lack of Windows support (there has been
      [a call to arms](https://www.reddit.com/r/haskell/comments/7tutxa/vty_needs_your_help_supporting_windows/) recently;
      I'd be happy if my findings with _terminal_ could help improve the situation with _vty_).
    - Dependency on `terminfo`.
    - No proper signal handling.

### brick

  - [brick](https://hackage.haskell.org/package/brick) is library on top of _vty_. Its
    scope is different from what _terminal_ does.

