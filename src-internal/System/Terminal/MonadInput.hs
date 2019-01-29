module System.Terminal.MonadInput where

import           Control.Applicative ((<|>))
import           Control.Monad (void, when)
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.List

type Row     = Int
type Rows    = Int
type Column  = Int
type Columns = Int

-- | This monad describes an environment that maintains a stream of `Event`s
--   and offers out-of-band signaling for interrupts.
--
--  * An interrupt shall occur if the user either presses CTRL+C
--    or any other mechanism the environment designates for that purpose.
--  * Implementations shall maintain an signal flag that is set
--    when a signal occurs. Computations in this monad shall check and
--    reset this flag regularly. If the execution environment finds this
--    flag still set when trying to signal another interrupt, it shall
--    throw `Control.Exception.AsyncException.UserInterrupt` to the
--    seemingly unresponsive computation.
--  * When a signal occurs, an `Event.SignalEvent`
--    must be added to the event stream in the same transaction.
--    This allows to flush all unprocessed events from the stream that
--    occured before the interrupt.
class (MonadIO m) => MonadInput m where
  -- | Wait for the next interrupt or next event transformed by a given mapper.
  --
  -- * The first mapper parameter is a transaction that succeeds as
  --   soon as an interrupt occurs. Executing this transaction
  --   resets the interrupt flag. When a second interrupt occurs before
  --   the interrupt flag has been reset, the current thread shall
  --   receive an `Control.Exception.AsyncException.UserInterrupt`.
  -- * The second mapper parameter is a transaction that succeeds as
  --   as soon as the next event arrives and removes that event from the
  --   stream of events. It shall be executed at most once within a single
  --   transaction or the transaction would block until the requested number
  --   of events is available.
  -- * NB: For each interrupt an `Interrupt` event occurs in the event stream.
  --   Take caution in order not to handle them twice.
  -- * The mapper may also be used in order to additionally wait on external
  --   events (like an `Control.Monad.Async.Async` to complete).
    waitInterruptOrEvent :: (STM () -> STM Event -> STM a) -> m a

-- | Wait for the next event.
--
-- * Returns as soon as an event occurs.
-- * This operation resets the interrupt flag, signaling responsiveness to
--   the execution environment. Be careful: There might be several events
--   preceding the `Interrupt` event in the event stream. FIXME
waitEvent :: MonadInput m => m Event
waitEvent = waitInterruptOrEvent $ \intr evs-> do
    ev <- evs
    when (ev == SignalEvent Interrupt) (intr <|> pure ())
    pure ev

-- | Wait for the next event or a given transaction.
--
-- * Returns as soon as an event occurs or the transaction succeeds.
-- * This operation resets the interrupt flag when encountering an interrupt,
--   signaling responsiveness to the execution environment.
-- * `Interupt`s occur in the event stream at their correct
--   position wrt to ordering of events. This is eventually not desired
--   when trying to handle interrupts with highest priority and
--  `waitSignalOrElse` should be considered then.
waitEventOrElse :: MonadInput m => STM a -> m (Either Event a)
waitEventOrElse stma = waitInterruptOrEvent $ \intr evs -> do
    let waitEvent = do
            ev <- evs
            when (ev == SignalEvent Interrupt) (intr <|> pure ())
            pure (Left ev)
    let waitElse = Right <$> stma
    waitEvent <|> waitElse

-- | Wait simultaneously for the next interrupt or a given transaction.
--
-- * Returns `Nothing` on interrupt and `Just` when the supplied
--   transaction succeeds first.
-- * This operation resets the interrupt flag, signaling responsiveness
--   to the execution environment.
-- * All pending events up to the interrupt are dropped in case of an interrupt.
waitInterruptOrElse :: MonadInput m => STM a -> m (Maybe a)
waitInterruptOrElse stma = waitInterruptOrEvent $ \intr evs -> do
    let waitInterrupt = do
            intr
            dropPending evs
            pure Nothing
    let waitElse = Just <$> stma
    waitInterrupt <|> waitElse
    where
        dropPending:: STM Event -> STM ()
        dropPending evs = ((Just <$> evs) <|> pure Nothing) >>= \case
            Just (SignalEvent Interrupt) -> pure ()
            Just ev                      -> dropPending evs
            Nothing                      -> pure ()

-- | Check whether an interrupt is pending.
--
-- * This operation resets the interrupt flag, signaling responsiveness
--   to the execution environment.
-- * All pending events up to the interrupt are dropped in case of an interrupt.
checkInterrupt :: MonadInput m => m Bool
checkInterrupt = waitInterruptOrElse (pure ()) >>= \case
    Nothing -> pure True
    _       -> pure False

data Event
    = KeyEvent Key Modifiers
    | MouseEvent MouseEvent
    | WindowEvent WindowEvent
    | DeviceEvent DeviceEvent
    | SignalEvent Signal
    | OtherEvent String
    deriving (Eq,Ord,Show)

data Key
    = CharKey Char
    | TabKey
    | SpaceKey
    | BackspaceKey
    | EnterKey
    | InsertKey
    | DeleteKey
    | HomeKey
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

instance Semigroup Modifiers where
    Modifiers a <> Modifiers b = Modifiers (a .|. b)

instance Monoid Modifiers where
    mempty = Modifiers 0

instance Show Modifiers where
    show (Modifiers 0) = "mempty"
    show (Modifiers 1) = "shiftKey"
    show (Modifiers 2) = "ctrlKey"
    show (Modifiers 4) = "altKey"
    show (Modifiers 8) = "metaKey"
    show i = "(" ++ intercalate " <> " ls ++ ")"
        where
        ls = foldl (\acc x-> if x .&. i /= mempty then show x:acc else acc) []
                    [metaKey, altKey, ctrlKey, shiftKey]

shiftKey, ctrlKey, altKey, metaKey :: Modifiers
shiftKey = Modifiers 1
ctrlKey  = Modifiers 2
altKey   = Modifiers 4
metaKey  = Modifiers 8

data MouseEvent
  = MouseMoved          (Int,Int)
  | MouseButtonPressed  (Int,Int) MouseButton
  | MouseButtonReleased (Int,Int) MouseButton
  | MouseButtonClicked  (Int,Int) MouseButton
  | MouseWheeled        (Int,Int) Direction
  deriving (Eq,Ord,Show)

data MouseButton
  = LeftMouseButton
  | RightMouseButton
  | OtherMouseButton
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

data DeviceEvent
  = DeviceAttributesReport String
  | CursorPositionReport (Row, Column)
  deriving (Eq, Ord, Show)

data Signal
  = Interrupt
  | OtherSignal BS.ByteString
  deriving (Eq, Ord, Show)
