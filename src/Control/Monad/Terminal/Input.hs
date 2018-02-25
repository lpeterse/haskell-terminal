{-# LANGUAGE LambdaCase #-}
module Control.Monad.Terminal.Input where

import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.ByteString

-- | This monad describes an environment that offers a stream of `Event`s
--   and an out-of-band signaling for interrupts.
--
--     * An interrupt shall occur if the user either presses CTRL+C
--       or any other mechanism the environment designates for that purpose.
--     * Implementations shall maintain an interrupt flag that is set
--       when an interrupt occurs. Computations in this monad shall check and
--       reset this flag regularly. If the execution environment finds this
--       flag still set when trying to signal another interrupt, it shall
--       throw `Control.Exception.AsyncException.UserInterrupt` to the
--       seemingly unresponsive computation.
--     * When an interrupt is signaled through the flag, an `Event.InterruptEvent`
--       must be added to the event stream in the same transaction.
--       This allows to flush all unprocessed events from the stream that
--       occured before the interrupt.
class (MonadIO m) => MonadInput m where
  -- | Wait for the next interrupt or the next event transformed by a given mapper.
  --
  --    * The first mapper parameter is a transaction that succeeds as
  --      soon as the interrupt flag gets set. Executing this transaction
  --      resets the interrupt flag. If the interrupt flag is not reset
  --      before a second interrupt occurs, the current thread shall
  --      receive an `Control.Exception.AsyncException.UserInterrupt`.
  --    * The second mapper parameter is a transaction that succeeds as
  --      as soon as the next event arrives and removes that event from the
  --      stream of events. It may be executed several times within the same
  --      transaction, but might not succeed every time.
  waitMapInterruptAndEvents :: (STM () -> STM Event -> STM a) -> m a

-- | Wait for the next event transformed by a given mapper.
--
--    * This operation resets the interrupt flag if the transaction succeeds,
--      signaling responsiveness to the execution environment.
--    * `Event.InterruptEvent`s occur in the event stream at their correct
--      position wrt to ordering of events. This is eventually not desired
--      when trying to handle interrupts with highest priority.
waitMapEvents :: MonadInput m => (STM Event -> STM a) -> m a
waitMapEvents f = waitMapInterruptAndEvents $ \intr evs->
  (intr `orElse` pure ()) >> f evs

--waitEventOrElse :: MonadInput m => STM a -> m (Maybe (Either Event a))
--waitEventOrElse stma =

-- | Wait simultaneously for the next interrupt or a given transaction.
--
--    * Returns `Nothing` on interrupt and `Just` when the supplied transaction
--      succeeds first.
--    * This operation resets the interrupt flag, signaling responsiveness
--      to the execution environment.
--    * All pending events up to and including the `InterruptEvent` are flushed
--      from the event stream in case of an interrupt.
waitInterruptOrElse :: MonadInput m => STM a -> m (Maybe a)
waitInterruptOrElse stma = waitMapInterruptAndEvents $ \intr evs->
  (intr >> dropTillInterruptEvent evs >> pure Nothing) `orElse` (Just <$> stma)
  where
    dropTillInterruptEvent :: STM Event -> STM ()
    dropTillInterruptEvent evs = evs >>= \case
      InterruptEvent -> pure ()
      -- It is very critical that the `MonadInput` instance is correct wrt to
      -- putting an `InterruptEvent` into the stream for every interrupt.
      -- If an implementation fails to fulfill this requirement one will
      -- encounter a "thread blocked indefinitely in an STM transaction"
      -- here.
      _              -> dropTillInterruptEvent evs

waitEvent        :: MonadInput m => m Event
waitEvent         = waitMapEvents id

waitInterrupt    :: MonadInput m => m ()
waitInterrupt     = waitInterruptOrElse retry >> pure ()

pollEvent        :: MonadInput m => m (Maybe Event)
pollEvent         = waitMapEvents $ \ev-> (Just <$> ev) `orElse` pure Nothing

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
  | InterruptEvent
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
