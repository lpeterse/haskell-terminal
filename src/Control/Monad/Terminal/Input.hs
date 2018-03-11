{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Control.Monad.Terminal.Input where

import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Bits
import           Data.List

-- | This monad describes an environment that maintains a stream of `Event`s
--   and offers out-of-band signaling for interrupts.
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

-- | Wait for the next event.
--
--    * Returns as soon as an event occurs.
--    * This operation resets the interrupt flag it returns,
--      signaling responsiveness to the execution environment.
--    * `Event.InterruptEvent`s occur in the event stream at their correct
--      position wrt to ordering of events. They are returned as regular
--      events. This is eventually not desired when trying to handle interrupts
--      with highest priority and `waitInterruptOrElse` should be considered then.
waitEvent :: MonadInput m => m Event
waitEvent = waitMapInterruptAndEvents $ \intr evs->
  (intr `orElse` pure ()) >> evs

-- | Wait simultaneously for the next event or a given transaction.
--
--    * Returns as soon as either an event occurs or the given transaction
--      succeeds.
--    * This operation resets the interrupt flag whenever it returns,
--      signaling responsiveness to the execution environment.
--    * `Event.InterruptEvent`s occur in the event stream at their correct
--      position wrt to ordering of events. They are returned as regular
--      events. This is eventually not desired when trying to handle interrupts
--      with highest priority and `waitInterruptOrElse` should be considered then.
waitEventOrElse :: MonadInput m => STM a -> m (Either Event a)
waitEventOrElse stma = waitMapInterruptAndEvents $ \intr evs->
  (intr `orElse` pure ()) >> ((Prelude.Left <$> evs) `orElse` (Prelude.Right <$> stma))

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
    dropTillInterruptEvent evs = ((Just <$> evs) `orElse` pure Nothing) >>= \case
      Nothing             -> pure ()
      Just InterruptEvent -> pure ()
      _                   -> dropTillInterruptEvent evs

data Key
  = CharKey Char
  | TabKey
  | SpaceKey
  | EnterKey
  | EraseKey
  | InsertKey
  | DeleteKey
  | HomeKey      -- ^ Pos 1
  | BeginKey
  | EndKey
  | PageUpKey
  | PageDownKey
  | EscapeKey
  | PrintKey
  | PauseKey
  | ArrowKey Direction
  | FunctionKey Int
  deriving (Eq,Ord,Show)

newtype Modifiers = Modifiers Int
  deriving (Eq, Ord, Bits)

instance Monoid Modifiers where
  mempty = Modifiers 0
  mappend (Modifiers a) (Modifiers b) = Modifiers (a .|. b)

instance Show Modifiers where
  show (Modifiers 0) = "mempty"
  show (Modifiers 1) = "shiftKey"
  show (Modifiers 2) = "ctrlKey"
  show (Modifiers 4) = "altKey"
  show (Modifiers 8) = "metaKey"
  show i = "(" ++ intercalate " .|. " ls ++ ")"
    where
      ls = foldl (\acc x-> if x .&. i /= mempty then show x:acc else acc) []
                 [metaKey, altKey, ctrlKey, shiftKey]

shiftKey, ctrlKey, altKey, metaKey :: Modifiers
shiftKey = Modifiers 1
ctrlKey  = Modifiers 2
altKey   = Modifiers 4
metaKey  = Modifiers 8

data Event
  = KeyEvent Key Modifiers
  | MouseEvent MouseEvent
  | WindowEvent WindowEvent
  | EvCursorPosition (Int,Int)
  | InterruptEvent
  | OtherEvent String
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
  = Upwards
  | Downwards
  | Leftwards
  | Rightwards
  deriving (Eq,Ord,Show)

data WindowEvent
  = WindowLostFocus
  | WindowGainedFocus
  | WindowSizeChanged (Int,Int)
  deriving (Eq, Ord, Show)
