module Control.Monad.Terminal.Events where

import           Data.ByteString

data Key
  = KNull
  | KChar Char
  | KEnter
  | KErase
  | KDelete
  | KEscape
  | KTab
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
  | MouseEvent  MouseEvent
  | WindowEvent WindowEvent
  | EvCursorPosition (Int,Int)
  | EvUnknownSequence String
  deriving (Eq,Ord,Show)

data MouseEvent
  = MouseMoved          (Int,Int)
  | MouseButtonPressed  (Int,Int) Button
  | MouseButtonReleased (Int,Int) Button
  | MouseButtonClicked  (Int,Int) Button
  | MouseWheeled        (Int,Int) Direction
  deriving (Eq,Ord,Show)

data Button
  = LeftButton
  | RightButton
  | OtherButton
  deriving (Eq,Ord,Show)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Eq,Ord,Show)

data WindowEvent
  = WindowLostFocus
  | WindowGainedFocus
  | WindowSizeChanged (Int,Int)
  deriving (Eq, Ord, Show)
