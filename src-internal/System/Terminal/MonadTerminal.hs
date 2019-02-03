module System.Terminal.MonadTerminal where

import           System.Terminal.MonadInput
import           System.Terminal.MonadPrinter
import           System.Terminal.MonadScreen

class (MonadInput m, MonadFormattingPrinter m, MonadColorPrinter m, MonadScreen m) => MonadTerminal m where
