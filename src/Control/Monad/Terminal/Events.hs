module Control.Monad.Terminal.Events where

import           Data.ByteString

data Key
  = KNull
  | KChar Char
  | KEnter
  | KErase
  | KDelete
  | KEsc
  | KTab Int
  | KBacktab Int
  | KLeft Int | KRight Int | KUp Int | KDown Int
  | KUpLeft | KUpRight | KDownLeft | KDownRight | KCenter
  | KFun Int | KPrtScr | KPause | KInsert
  | KHome | KPageUp | KEnd | KPageDown | KBegin | KMenu
  deriving (Eq,Ord,Show)

data Signal
  = Interrupt
  deriving (Eq,Ord,Show)

-- | Modifier keys. Key codes are interpreted such that users are more
-- likely to have Meta than Alt; for instance on the PC Linux console,
-- 'MMeta' will generally correspond to the physical Alt key.
data Modifier = MShift | MCtrl | MMeta | MAlt
  deriving (Eq,Ord,Show)

-- | Events.
data Event
  = EvKey Key [Modifier]
  -- ^ A keyboard key was pressed with the specified modifiers.
  | EvMouseDown Int Int Button [Modifier]
  -- ^ A mouse button was pressed at the specified column and row. Any
  -- modifiers available in the event are also provided.
  | MouseEvent MouseEvent
  -- ^ A mouse button was released at the specified column and
  -- row. Some terminals report only that a button was released
  -- without specifying which one; in that case, Nothing is provided.
  -- Otherwise Just the button released is included in the event.
  | EvResize (Int, Int)
  -- ^ If read from 'eventChannel' this is the size at the time of the
  -- signal. If read from 'nextEvent' this is the size at the time the
  -- event was processed by Vty. Typically these are the same, but if
  -- somebody is resizing the terminal quickly they can be different.
  | EvPaste ByteString
  -- ^ A paste event occurs when a bracketed paste input sequence is
  -- received. For terminals that support bracketed paste mode, these
  -- events will be triggered on a paste event. Terminals that do not
  -- support bracketed pastes will send the paste contents as ordinary
  -- input (which is probably bad, so beware!) Note that the data is
  -- provided in raw form and you'll have to decode (e.g. as UTF-8) if
  -- that's what your application expects.
  | FocusEvent
  -- ^ The terminal running the application gained input focus.
  | EvCursorPosition (Int,Int)
  | EvUnknownSequence String
  deriving (Eq,Ord,Show)

data MouseEvent
  = MouseMoved          (Int,Int)
  | MouseButtonPressed  (Int,Int) Button
  | MouseButtonReleased (Int,Int)
  | MouseWheeled        (Int,Int) Direction
  deriving (Eq,Ord,Show)

data Button
  = LeftButton
  | RightButton
  | OtherButton
  deriving (Eq,Ord,Show)

data Click
  = SingleClick
  | DoubleClick
  deriving (Eq,Ord,Show)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Eq,Ord,Show)
