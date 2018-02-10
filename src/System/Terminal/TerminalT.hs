{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module System.Terminal.TerminalT
  ( TerminalT ()
  , runTerminalT
  )
where

import           Control.Concurrent
import qualified Control.Concurrent.Async     as A
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception            as E
import           Control.Monad                (forever, when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bits
import qualified Data.ByteString              as BS
import           Data.Char
import           Data.Function                (fix)
import           Data.Maybe
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import           Data.Word
import           System.Environment
import qualified System.IO                    as IO

import qualified System.Terminal.Class        as T
import qualified System.Terminal.Color        as T
import qualified System.Terminal.Events       as T
import qualified System.Terminal.Modes        as T
import qualified System.Terminal.Platform     as T
import qualified System.Terminal.Pretty       as T

newtype TerminalT m a
  = TerminalT (StateT TerminalState (ReaderT (STM T.Event) m) a)
  deriving (Functor, Applicative, Monad, MonadIO)

runTerminalT :: TerminalT IO a -> IO a
runTerminalT (TerminalT ma) = T.withTerminal $ do
  eventChan     <- newTChanIO
  interruptFlag <- newTVarIO False
  withoutEcho $
    withRawMode $
      T.withHookedInterruptSignal $ \waitSignal->
        A.withAsync (runInput $ writeTChan eventChan) $ \inputThread->
        A.withAsync (runAction interruptFlag $ readTChan eventChan) $ \actionThread->
        fix $ \proceed-> do
          let waitInput  = A.waitSTM inputThread >> pure Nothing
          let waitAction = Just <$> A.waitSTM actionThread
          let waitInterrupt = waitSignal >> swapTVar interruptFlag True >>= \case
                True  -> throwSTM E.UserInterrupt -- No one has handled an earlier interrupt - exit!
                False -> pure Nothing
          -- The input thread is designed to never return, but it might
          -- throw an exception which will be rethrown automatically.
          -- The action thread might return which can be detected through the
          -- `Just` constructor. It might also throw an exception.
          -- `waitInterrupt` waits for an incoming incoming interrupt signal.
          -- It always tries to forward it to the action thread, but
          -- if it finds a previous interrupt to not been handled it assumes
          -- the action thread is too busy or deadlocked and throws a
          -- `E.UserInterrupt` exception terminating everything.
          -- This assures that the application always responds to Ctrl-C.
          atomically (waitInput `orElse` waitAction `orElse` waitInterrupt) >>= \case
                Just a  -> pure a
                Nothing -> proceed
  where
    withoutEcho :: IO a -> IO a
    withoutEcho = bracket
      (IO.hGetEcho IO.stdin >>= \x-> IO.hSetEcho IO.stdin False >> pure x)
      (IO.hSetEcho IO.stdin) . const

    withRawMode :: IO a -> IO a
    withRawMode = bracket
      (IO.hGetBuffering IO.stdin >>= \b-> IO.hSetBuffering IO.stdin IO.NoBuffering >> pure b)
      (IO.hSetBuffering IO.stdin) . const

    runInput :: (T.Event -> STM ()) -> IO ()
    runInput pushEvent = flip evalStateT BS.empty $ forever $ do
      ev <- decodeAnsi
      liftIO $ atomically $ pushEvent ev

    -- runAction :: TVar Bool -> STM T.Event -> IO _
    runAction interruptFlag =
      runReaderT (evalStateT ma defaultTerminalState)

data TerminalState
  = TerminalState
  { tsInput      :: BS.ByteString
  , tsAttributes :: TerminalStateAttributes
  } deriving (Eq, Ord, Show)

data TerminalStateAttributes
  = TerminalStateAttributes
  { tsNegative        :: !Bool
  , tsUnderline       :: !Bool
  , tsCursorVisible   :: !Bool
  , tsForegroundColor :: !T.Color
  , tsBackgroundColor :: !T.Color
  } deriving (Eq, Ord, Show)

defaultTerminalState :: TerminalState
defaultTerminalState =
  TerminalState
  { tsInput           = mempty
  , tsAttributes      = defaultTerminalStateAttributes
  }

defaultTerminalStateAttributes :: TerminalStateAttributes
defaultTerminalStateAttributes =
  TerminalStateAttributes
  { tsNegative        = False
  , tsUnderline       = False
  , tsCursorVisible   = True
  , tsForegroundColor = T.ColorDefault
  , tsBackgroundColor = T.ColorDefault
  }

modAttrs :: Monad m => (TerminalStateAttributes -> TerminalStateAttributes) -> TerminalT m ()
modAttrs f = TerminalT $ do
  st <- get
  put st { tsAttributes = f (tsAttributes st) }

modFgColor :: Monad m => T.Color -> TerminalT m ()
modFgColor c = modAttrs (\as -> as { tsForegroundColor = c })

modBgColor :: Monad m => T.Color -> TerminalT m ()
modBgColor c = modAttrs (\as -> as { tsBackgroundColor = c })

modUnderline :: Monad m => Bool -> TerminalT m ()
modUnderline b = modAttrs (\as -> as { tsUnderline = b })

modNegative :: Monad m => Bool -> TerminalT m ()
modNegative b = modAttrs (\as -> as { tsNegative = b })

instance (MonadIO m, MonadMask m) => T.MonadTerminal (TerminalT m) where

instance MonadIO m => T.MonadEvent (TerminalT m) where
  withEventSTM f = TerminalT $ do
    ev <- lift ask
    liftIO $ atomically $ f ev

instance (MonadIO m, MonadMask m) => T.MonadIsolate (TerminalT m) where
  isolate (TerminalT ma) = TerminalT $ bracket get
    (\st1-> do
        st2 <- get
        ev  <- lift ask
        let TerminalT nb = putDiff (tsAttributes st1) (tsAttributes st2)
        runReaderT (evalStateT nb st2) ev
        put st2 { tsAttributes = tsAttributes st1 }
    )
    ( const ma )
    where
      putDiff a b = do
        when (tsNegative a  /= tsNegative  b) $ if tsNegative a then T.setNegative else T.setPositive
        when (tsUnderline a /= tsUnderline b) $ T.setUnderline (tsUnderline a)
        when (tsForegroundColor a /= tsForegroundColor b) $ T.setForegroundColor (tsForegroundColor a)
        when (tsBackgroundColor a /= tsBackgroundColor b) $ T.setBackgroundColor (tsBackgroundColor a)
        when (tsCursorVisible   a /= tsCursorVisible   b) $ if tsCursorVisible a then T.cursorShow else T.cursorHide

instance (MonadIO m, MonadMask m) => T.MonadPrinter (TerminalT m) where
  putChar c = liftIO $ when (isPrint c || isSpace c) $ IO.putChar c
  putString = liftIO . IO.putStr . filter (\c-> isPrint c || isSpace c)
  putStringLn s = T.putString s >> T.putLn
  putText = liftIO . Text.putStr . Text.filter (\c-> isPrint c || isSpace c)
  putTextLn t = T.putText t >> T.putLn
  putLn = liftIO $ IO.putStr "\n"

  flush = liftIO $ IO.hFlush IO.stdout

instance (MonadIO m, MonadMask m) => T.MonadColorPrinter (TerminalT m) where
  setDefault                                       = liftIO $ IO.putStr "\ESC[m"
  setForegroundColor c@T.ColorDefault              = modFgColor c >> liftIO (IO.putStr "\ESC[39m")
  setForegroundColor c@(T.Color4Bit T.Black     False) = modFgColor c >> liftIO (IO.putStr "\ESC[30m")
  setForegroundColor c@(T.Color4Bit T.Red       False) = modFgColor c >> liftIO (IO.putStr "\ESC[31m")
  setForegroundColor c@(T.Color4Bit T.Green     False) = modFgColor c >> liftIO (IO.putStr "\ESC[32m")
  setForegroundColor c@(T.Color4Bit T.Yellow    False) = modFgColor c >> liftIO (IO.putStr "\ESC[33m")
  setForegroundColor c@(T.Color4Bit T.Blue      False) = modFgColor c >> liftIO (IO.putStr "\ESC[34m")
  setForegroundColor c@(T.Color4Bit T.Magenta   False) = modFgColor c >> liftIO (IO.putStr "\ESC[35m")
  setForegroundColor c@(T.Color4Bit T.Cyan      False) = modFgColor c >> liftIO (IO.putStr "\ESC[36m")
  setForegroundColor c@(T.Color4Bit T.White     False) = modFgColor c >> liftIO (IO.putStr "\ESC[37m")
  setForegroundColor c@(T.Color4Bit T.Black      True) = modFgColor c >> liftIO (IO.putStr "\ESC[90m")
  setForegroundColor c@(T.Color4Bit T.Red        True) = modFgColor c >> liftIO (IO.putStr "\ESC[91m")
  setForegroundColor c@(T.Color4Bit T.Green      True) = modFgColor c >> liftIO (IO.putStr "\ESC[92m")
  setForegroundColor c@(T.Color4Bit T.Yellow     True) = modFgColor c >> liftIO (IO.putStr "\ESC[93m")
  setForegroundColor c@(T.Color4Bit T.Blue       True) = modFgColor c >> liftIO (IO.putStr "\ESC[94m")
  setForegroundColor c@(T.Color4Bit T.Magenta    True) = modFgColor c >> liftIO (IO.putStr "\ESC[95m")
  setForegroundColor c@(T.Color4Bit T.Cyan       True) = modFgColor c >> liftIO (IO.putStr "\ESC[96m")
  setForegroundColor c@(T.Color4Bit T.White      True) = modFgColor c >> liftIO (IO.putStr "\ESC[97m")
  setBackgroundColor c@T.ColorDefault              = modBgColor c >> liftIO (IO.putStr "\ESC[49m")
  setBackgroundColor c@(T.Color4Bit T.Black     False) = modBgColor c >> liftIO (IO.putStr "\ESC[40m")
  setBackgroundColor c@(T.Color4Bit T.Red       False) = modBgColor c >> liftIO (IO.putStr "\ESC[41m")
  setBackgroundColor c@(T.Color4Bit T.Green     False) = modBgColor c >> liftIO (IO.putStr "\ESC[42m")
  setBackgroundColor c@(T.Color4Bit T.Yellow    False) = modBgColor c >> liftIO (IO.putStr "\ESC[43m")
  setBackgroundColor c@(T.Color4Bit T.Blue      False) = modBgColor c >> liftIO (IO.putStr "\ESC[44m")
  setBackgroundColor c@(T.Color4Bit T.Magenta   False) = modBgColor c >> liftIO (IO.putStr "\ESC[45m")
  setBackgroundColor c@(T.Color4Bit T.Cyan      False) = modBgColor c >> liftIO (IO.putStr "\ESC[46m")
  setBackgroundColor c@(T.Color4Bit T.White     False) = modBgColor c >> liftIO (IO.putStr "\ESC[47m")
  setBackgroundColor c@(T.Color4Bit T.Black      True) = modBgColor c >> liftIO (IO.putStr "\ESC[100m")
  setBackgroundColor c@(T.Color4Bit T.Red        True) = modBgColor c >> liftIO (IO.putStr "\ESC[101m")
  setBackgroundColor c@(T.Color4Bit T.Green      True) = modBgColor c >> liftIO (IO.putStr "\ESC[102m")
  setBackgroundColor c@(T.Color4Bit T.Yellow     True) = modBgColor c >> liftIO (IO.putStr "\ESC[103m")
  setBackgroundColor c@(T.Color4Bit T.Blue       True) = modBgColor c >> liftIO (IO.putStr "\ESC[104m")
  setBackgroundColor c@(T.Color4Bit T.Magenta    True) = modBgColor c >> liftIO (IO.putStr "\ESC[105m")
  setBackgroundColor c@(T.Color4Bit T.Cyan       True) = modBgColor c >> liftIO (IO.putStr "\ESC[106m")
  setBackgroundColor c@(T.Color4Bit T.White      True) = modBgColor c >> liftIO (IO.putStr "\ESC[107m")
  setUnderline                               True  = modUnderline True  >> liftIO (IO.putStr "\ESC[4m")
  setUnderline                               False = modUnderline False >> liftIO (IO.putStr "\ESC[24m")
  setPositive                                      = modNegative  False >> liftIO (IO.putStr "\ESC[27m")
  setNegative                                      = modNegative  True  >> liftIO (IO.putStr "\ESC[7m")

instance (MonadIO m, MonadMask m) => T.MonadScreen (TerminalT m) where
  clear                 = liftIO $ IO.putStr "\ESC[H"
  cursorUp i            = liftIO $ IO.putStr $ "\ESC[" ++ show (safeN i) ++ "A"
  cursorDown i          = liftIO $ IO.putStr $ "\ESC[" ++ show (safeN i) ++ "B"
  cursorForward i       = liftIO $ IO.putStr $ "\ESC[" ++ show (safeN i) ++ "C"
  cursorBackward i      = liftIO $ IO.putStr $ "\ESC[" ++ show (safeN i) ++ "D"
  cursorPosition x y    = liftIO $ IO.putStr $ "\ESC[" ++ show (safeN x) ++  ";" ++ show (safeN y) ++ "H"
  cursorHide            = liftIO $ IO.putStr "\ESC[?25l"
  cursorShow            = liftIO $ IO.putStr "\ESC[?25h"

safeN :: Int -> Int
safeN n
  | n >= 1 && n <= 32767 = n
  | otherwise            = 1

-------------------------------------------------------------------------------
-- MonadInput
-------------------------------------------------------------------------------

class Monad m => MonadInput m where
  askModes        :: m T.TermModes
  getNext         :: m Word8
  getNextNonBlock :: m (Maybe Word8)
  wait            :: m ()

instance MonadInput (StateT BS.ByteString IO) where
  askModes = do
    term <- liftIO $ lookupEnv "TERM"
    pure $ fromMaybe T.termModesDefault (T.termModes <$> term)
  getNext = do
    st <- get
    case BS.uncons st of
      Just (b,bs) -> put bs >> pure b
      Nothing -> do
        ccs <- liftIO $ BS.hGetSome IO.stdin 1024
        put (BS.tail ccs)
        pure (BS.head ccs)
  getNextNonBlock = do
    st <- get
    case BS.uncons st of
      Just (b,bs) -> put bs >> pure (Just b)
      Nothing -> do
        ccs <- liftIO $ BS.hGetNonBlocking IO.stdin 1024
        case BS.uncons ccs of
          Just (c,cs) -> put cs >> pure (Just c)
          Nothing     -> pure Nothing
  wait = liftIO $ threadDelay 100000

decodeAnsi :: MonadInput m => m T.Event
decodeAnsi = decode1 =<< getNext
  where
    decode1 :: MonadInput m => Word8 -> m T.Event
    decode1 x
      -- The first 31 values are control codes.
      -- They are mapped as lower case letter + MCtrl modifier.
      | x ==  0   = pure $ T.EvKey T.KNull  []
      -- | x <= 26   = pure $ T.EvKey (T.KChar [] $ toEnum $ 96 + fromIntegral x) [T.MCtrl]
      | x <= 26   = pure $ T.EvKey (T.KChar [] $ toEnum $ fromIntegral x) []
      -- The escape control code might or might not introduce an escape sequencT.
      -- `decodeEscape` handles this by analysing the timing.
      | x == 27   = decodeEscape
      -- The next 4 are '\\', ']', '^' and '-'.
      -- They have a different offset as they are above the upper case letters.
      | x == 28   = pure $ T.EvKey (T.KChar [] '\\') [T.MCtrl]
      | x == 29   = pure $ T.EvKey (T.KChar [] ']')  [T.MCtrl]
      | x == 30   = pure $ T.EvKey (T.KChar [] '^')  [T.MCtrl]
      | x == 31   = pure $ T.EvKey (T.KChar [] '_')  [T.MCtrl]
      -- All following values are printable except 127 which is interpreted as backspace (terminal dependant!)
      | x == 127  = pure $ T.EvKey (T.KChar [] '\DEL') []
      | otherwise = flip T.EvKey [] . T.KChar [] <$> decodeUtf8Sequence x

    decodeEscape :: MonadInput m => m T.Event
    decodeEscape = getNextNonBlock >>= \case
      Nothing -> do
        wait -- a single escape can only be distinguished by timing
        getNextNonBlock >>= \case
          Nothing -> pure $ T.EvKey (T.KChar [T.MCtrl] '[') []
          Just x  -> decodeEscapeSequence x
      Just x  -> decodeEscapeSequence x

    decodeEscapeSequence :: (MonadInput m) => Word8 -> m T.Event
    decodeEscapeSequence x
      | x >= 1 && x <= 25 = pure $ T.EvKey (T.KChar [] $ toEnum $ 64 + fromIntegral x) [T.MCtrl, T.MAlt] -- urxvt
      | x == 27   = getNext >>= \case
                      79 -> getNext >>= \case -- seems to just add MAlt to all sequences
                        97  -> pure $ T.EvKey (T.KUp    1) [T.MCtrl, T.MAlt] -- urxvt
                        98  -> pure $ T.EvKey (T.KDown  1) [T.MCtrl, T.MAlt] -- urxvt
                        99  -> pure $ T.EvKey (T.KRight 1) [T.MCtrl, T.MAlt] -- urxvt
                        100 -> pure $ T.EvKey (T.KLeft  1) [T.MCtrl, T.MAlt] -- urxvt
                        y   -> unknownSequence [27, 27, 79, y]
                      91 -> getNext >>= \y-> decodeCSI y >>= \case
                        T.EvKey k ms -> pure $ T.EvKey k (T.MAlt:ms) -- urxvt
                        ev           -> pure ev
      | x == 31   = pure $ T.EvKey (T.KChar [] '-') [T.MAlt, T.MShift] -- urxvt
      | x == 33   = pure $ T.EvKey (T.KChar [] '1') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == 35   = pure $ T.EvKey (T.KChar [] '3') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == 36   = pure $ T.EvKey (T.KChar [] '4') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == 37   = pure $ T.EvKey (T.KChar [] '5') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == 38   = pure $ T.EvKey (T.KChar [] '7') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == 39   = pure $ T.EvKey (T.KChar [] '\'') [T.MAlt] -- urxvt, gnome-terminal
      | x == 40   = pure $ T.EvKey (T.KChar [] '9') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == 41   = pure $ T.EvKey (T.KChar [] '0') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == 42   = pure $ T.EvKey (T.KChar [] '8') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == 43   = pure $ T.EvKey (T.KChar [] '=') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == 44   = pure $ T.EvKey (T.KChar [] ',') [T.MAlt] -- urxvt, gnome-terminal
      | x == 45   = pure $ T.EvKey (T.KChar [] '-') [T.MAlt]
      | x == 46   = pure $ T.EvKey (T.KChar [] '.') [T.MAlt] -- urxvt, gnome-terminal
      | x == 47   = pure $ T.EvKey (T.KChar [] '/') [T.MAlt] -- urxvt, gnome-terminal
      | x == 48   = pure $ T.EvKey (T.KChar [] '0') [T.MAlt]
      | x == 49   = pure $ T.EvKey (T.KChar [] '1') [T.MAlt]
      | x == 50   = pure $ T.EvKey (T.KChar [] '2') [T.MAlt]
      | x == 51   = pure $ T.EvKey (T.KChar [] '3') [T.MAlt]
      | x == 52   = pure $ T.EvKey (T.KChar [] '4') [T.MAlt]
      | x == 53   = pure $ T.EvKey (T.KChar [] '5') [T.MAlt]
      | x == 54   = pure $ T.EvKey (T.KChar [] '6') [T.MAlt]
      | x == 55   = pure $ T.EvKey (T.KChar [] '7') [T.MAlt]
      | x == 56   = pure $ T.EvKey (T.KChar [] '8') [T.MAlt]
      | x == 57   = pure $ T.EvKey (T.KChar [] '9') [T.MAlt]
      | x == 59   = pure $ T.EvKey (T.KChar [] ';') [T.MAlt]
      | x == 60   = pure $ T.EvKey (T.KChar [] '<') [T.MAlt]
      | x == 61   = getNextNonBlock >>= \case
                      Nothing -> pure $ T.EvKey (T.KChar [] '=') [T.MAlt]
                      Just y  -> error (show y)
      | x == 62   = pure $ T.EvKey (T.KChar [] '<') [T.MAlt, T.MShift]
      | x == 64   = pure $ T.EvKey (T.KChar [] '2') [T.MAlt, T.MShift]
      | x == 65   = pure $ T.EvKey (T.KChar [] 'A') [T.MAlt, T.MShift]
      | x == 79   = getNext >>= \case
                      80  -> pure $ T.EvKey (T.KFun   1) [] -- gnome-terminal (?)
                      81  -> pure $ T.EvKey (T.KFun   2) [] -- gnome-terminal
                      82  -> pure $ T.EvKey (T.KFun   3) [] -- gnome-terminal
                      83  -> pure $ T.EvKey (T.KFun   4) [] -- gnome-terminal
                      97  -> pure $ T.EvKey (T.KUp    1) [T.MCtrl] -- urxvt
                      98  -> pure $ T.EvKey (T.KDown  1) [T.MCtrl] -- urxvt
                      99  -> pure $ T.EvKey (T.KRight 1) [T.MCtrl] -- urxvt
                      100 -> pure $ T.EvKey (T.KLeft  1) [T.MCtrl] -- urxvt
                      xs  -> error (show xs)
      | x == 91   = wait >> getNextNonBlock >>= \case
                      Nothing -> pure $ T.EvKey (T.KChar [] '[') [T.MAlt] -- urxvt, gnome-terminal
                      Just y  -> decodeCSI y
      | x == 92   = pure $ T.EvKey (T.KChar [] '\\') [T.MAlt] -- urxvt, gnome-terminal
      | x == 93   = pure $ T.EvKey (T.KChar [] ']') [T.MAlt] -- urxvt, gnome-terminal
      | x == 94   = pure $ T.EvKey (T.KChar [] '6') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == 95   = pure $ T.EvKey (T.KChar [] '_') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == 96   = pure $ T.EvKey (T.KChar [] '`') [T.MAlt] -- urxvt, gnome-terminal
      | x >= 97 && x <= 122 = pure $ T.EvKey (T.KChar [] $ toEnum $ fromIntegral x) [T.MAlt]
      | x == 126  = pure $ T.EvKey (T.KChar [] '`') [T.MAlt, T.MShift] -- urxvt, gnome-terminal
      | x == 127  = pure $ T.EvKey T.KDelete []
      | otherwise = error $ show x

    decodeCSI :: (MonadInput m) => Word8 -> m T.Event
    decodeCSI y = withParams1 y $ \ps-> \case
      27         -> pure $ T.EvKey (T.KChar [] '[') [T.MAlt]          -- urxvt
      36         -> pure $ T.EvKey T.KDelete [T.MAlt, T.MShift]       -- urxvt, gnome-terminal
      64 {- @ -} -> withN 1 ps $ \n-> pure $ T.EvKey (T.KSpace  n) [] -- in urxvt shift+ctrl+pageup/down causes n==5/6
      65 {- A -} -> decodeArrowKey ps T.KUp
      66 {- B -} -> decodeArrowKey ps T.KDown
      67 {- C -} -> decodeArrowKey ps T.KRight
      68 {- D -} -> decodeArrowKey ps T.KLeft
      69 {- E -} -> undefined
      70 {- F -} -> pure $ T.EvKey T.KEnd []
      71 {- G -} -> undefined
      72 {- H -} -> pure $ T.EvKey T.KHome []
      73 {- I -} -> withN 1 ps $ \n-> pure $ T.EvKey (T.KTab    n) []
      74 {- J -} -> undefined
      75 {- K -} -> undefined
      76 {- L -} -> undefined
      77 {- M -} -> undefined
      78 {- N -} -> undefined
      79 {- O -} -> undefined
      80 {- P -} -> pure $ T.EvKey (T.KFun 1) [T.MCtrl]
      81 {- Q -} -> pure $ T.EvKey (T.KFun 2) [T.MCtrl]
      82 {- R -} -> pure $ T.EvKey (T.KFun 3) [T.MCtrl]
      83 {- S -} -> pure $ T.EvKey (T.KFun 4) [T.MCtrl]
      84 {- T -} -> undefined
      85 {- U -} -> undefined
      86 {- V -} -> undefined
      87 {- W -} -> undefined
      88 {- X -} -> undefined
      89 {- Y -} -> undefined
      90 {- Z -} -> withN 1 ps $ \n-> pure $ T.EvKey (T.KBackTab n) []
      94  -> case ps of
        [    50] {-    -} -> pure $ T.EvKey T.KInsert   [T.MCtrl]
        [    51] {-  3 -} -> pure $ T.EvKey T.KDelete   [T.MCtrl]
        [    53] {-  4 -} -> pure $ T.EvKey T.KPageUp   [T.MCtrl]
        [    54] {-  4 -} -> pure $ T.EvKey T.KPageDown [T.MCtrl]
        [    55] {-  6 -} -> pure $ T.EvKey T.KHome     [T.MCtrl]
        [    56] {-  6 -} -> pure $ T.EvKey T.KEnd      [T.MCtrl]
        [49, 49] {- 11 -} -> pure $ T.EvKey (T.KFun  1) [T.MCtrl]
        [49, 50] {- 12 -} -> pure $ T.EvKey (T.KFun  2) [T.MCtrl]
        [49, 51] {- 13 -} -> pure $ T.EvKey (T.KFun  3) [T.MCtrl]
        [49, 52] {- 14 -} -> pure $ T.EvKey (T.KFun  4) [T.MCtrl]
        [49, 53] {- 15 -} -> pure $ T.EvKey (T.KFun  5) [T.MCtrl]
        [49, 55] {- 17 -} -> pure $ T.EvKey (T.KFun  6) [T.MCtrl]
        [49, 56] {- 18 -} -> pure $ T.EvKey (T.KFun  7) [T.MCtrl]
        [49, 57] {- 19 -} -> pure $ T.EvKey (T.KFun  8) [T.MCtrl]
        [50, 48] {- 20 -} -> pure $ T.EvKey (T.KFun  9) [T.MCtrl]
        [50, 49] {- 21 -} -> pure $ T.EvKey (T.KFun 10) [T.MCtrl]
        [50, 51] {- 22 -} -> pure $ T.EvKey (T.KFun 11) [T.MCtrl]
        [50, 52] {- 23 -} -> pure $ T.EvKey (T.KFun 12) [T.MCtrl]
        [50, 53] {- 24 -} -> pure $ T.EvKey (T.KFun 13) [T.MCtrl]
        [50, 54] {- 25 -} -> pure $ T.EvKey (T.KFun 14) [T.MCtrl]
        [50, 56] {- 27 -} -> pure $ T.EvKey (T.KFun 15) [T.MCtrl]
        [50, 57] {- 28 -} -> pure $ T.EvKey (T.KFun 16) [T.MCtrl]
        [51, 49] {- 31 -} -> pure $ T.EvKey (T.KFun 17) [T.MCtrl]
        [51, 50] {- 32 -} -> pure $ T.EvKey (T.KFun 18) [T.MCtrl]
        [51, 51] {- 33 -} -> pure $ T.EvKey (T.KFun 19) [T.MCtrl]
        [51, 52] {- 32 -} -> pure $ T.EvKey (T.KFun 20) [T.MCtrl]
        _        -> error ("FOOB" ++ show ps)
      102 -> undefined
      105 -> pure $ T.EvKey T.KPrtScr []
      109 -> undefined -- SGR
      126 -> case ps of
        [    50]      {-  2 -} -> pure $ T.EvKey T.KInsert []
        [    51]      {-  3 -} -> pure $ T.EvKey T.KDelete []
        [    53]      {-  5 -} -> pure $ T.EvKey T.KPageUp []
        [    54]      {-  6 -} -> pure $ T.EvKey T.KPageDown []
        [    55]      {-  9 -} -> pure $ T.EvKey T.KHome []
        [    56]      {- 10 -} -> pure $ T.EvKey T.KEnd []
        [49, 49]      {- 11 -} -> pure $ T.EvKey (T.KFun 1) []
        [49, 50]      {- 12 -} -> pure $ T.EvKey (T.KFun 2) []
        [49, 51]      {- 13 -} -> pure $ T.EvKey (T.KFun 3) []
        [49, 52]      {- 14 -} -> pure $ T.EvKey (T.KFun 4) []
        [49, 53]      {- 15 -} -> pure $ T.EvKey (T.KFun 5) []
        [49, 55]      {- 17 -} -> pure $ T.EvKey (T.KFun 6) []
        [49, 56]      {- 18 -} -> pure $ T.EvKey (T.KFun 7) []
        [49, 57]      {- 19 -} -> pure $ T.EvKey (T.KFun 8) []
        [50, 48]      {- 20 -} -> pure $ T.EvKey (T.KFun 9) []
        [50, 49]      {- 21 -} -> pure $ T.EvKey (T.KFun 10) []
        [50, 51]      {- 22 -} -> pure $ T.EvKey (T.KFun 11) []
        [50, 52]      {- 23 -} -> pure $ T.EvKey (T.KFun 12) []
        [50, 53]      {- 24 -} -> pure $ T.EvKey (T.KFun 13) []
        [50, 54]      {- 25 -} -> pure $ T.EvKey (T.KFun 14) []
        [50, 56]      {- 27 -} -> pure $ T.EvKey (T.KFun 15) []
        [50, 57]      {- 28 -} -> pure $ T.EvKey (T.KFun 16) []
        [51, 49]      {- 31 -} -> pure $ T.EvKey (T.KFun 17) []
        [51, 50]      {- 32 -} -> pure $ T.EvKey (T.KFun 18) []
        [51, 51]      {- 33 -} -> pure $ T.EvKey (T.KFun 19) []
        [51, 52]      {- 32 -} -> pure $ T.EvKey (T.KFun 20) []
        [49,53,59,53] {- .. -} -> pure $ T.EvKey (T.KFun  5) [T.MCtrl] -- gnome-terminal
        [49,55,59,53] {- .. -} -> pure $ T.EvKey (T.KFun  6) [T.MCtrl] -- gnome-terminal
        [49,56,59,53] {- .. -} -> pure $ T.EvKey (T.KFun  7) [T.MCtrl] -- gnome-terminal
        [49,57,59,53] {- .. -} -> pure $ T.EvKey (T.KFun  8) [T.MCtrl] -- gnome-terminal
        [50,48,59,53] {- .. -} -> pure $ T.EvKey (T.KFun  9) [T.MCtrl] -- gnome-terminal
        [50,49,59,53] {- .. -} -> pure $ T.EvKey (T.KFun 10) [T.MCtrl] -- gnome-terminal
        [50,51,59,53] {- .. -} -> pure $ T.EvKey (T.KFun 11) [T.MCtrl] -- gnome-terminal
        [50,52,59,53] {- .. -} -> pure $ T.EvKey (T.KFun 12) [T.MCtrl] -- gnome-terminal
        [50,59,51]    {- .. -} -> pure $ T.EvKey T.KInsert [T.MAlt]    -- gnome-terminal
        [51,59,53]    {- .. -} -> pure $ T.EvKey T.KDelete [T.MCtrl]   -- xterm
        [51,59,51]    {- .. -} -> pure $ T.EvKey T.KDelete [T.MAlt]    -- gnome-terminal
        [53,59,53]    {- .. -} -> pure $ T.EvKey T.KPageUp [T.MCtrl]   -- gnome-terminal
        [54,59,53]    {- .. -} -> pure $ T.EvKey T.KPageDown [T.MCtrl]   -- gnome-terminal
        [53,59,51]    {- .. -} -> pure $ T.EvKey T.KPageUp [T.MAlt]   -- gnome-terminal
        [54,59,51]    {- .. -} -> pure $ T.EvKey T.KPageDown [T.MAlt]   -- gnome-terminal
        [53,59,55]    {- .. -} -> pure $ T.EvKey T.KPageUp [T.MCtrl, T.MAlt]   -- gnome-terminal
        [54,59,55]    {- .. -} -> pure $ T.EvKey T.KPageDown [T.MCtrl, T.MAlt]   -- gnome-terminal
        _             {- .. -} -> error $ show ps
      x   -> error $ "HERE" ++ show x
      where
        decodeArrowKey ps key = withNumbers ps $ \case
          []    -> pure $ T.EvKey (key 1) []
          [n]   -> pure $ T.EvKey (key $ if n == 0 then 1 else n) []
          [1,3] -> pure $ T.EvKey (key 1) [T.MAlt]          -- gnome-terminal
          [1,5] -> pure $ T.EvKey (key 1) [T.MCtrl]         -- gnome-terminal
          [1,7] -> pure $ T.EvKey (key 1) [T.MCtrl, T.MAlt] -- gnome-terminal
          xs    -> error (show xs)

unknownSequence :: MonadInput m => [Word8] -> m a
unknownSequence xs = getNextNonBlock >>= \case
  Just x  -> unknownSequence $ xs ++ [x]
  Nothing -> error $ "unknown sequence " ++ show xs

withParams1 :: MonadInput m => Word8 -> ([Word8] -> Word8 -> m a) -> m a
withParams1 x f
  | x >= 0x30 && x <= 0x3f = withParameters 256 [x]
  | otherwise              = f [] x
  where
    withParameters 0 _            = fail "CSI: LENGTH LIMIT EXCEEDED"
    withParameters limit ps       = getNext >>= \case
      y | y >= 0x30 && y <= 0x3F -> withParameters (limit - 1) $! y:ps
        | otherwise              -> f (reverse ps) y

withN :: Monad m => Int -> [Word8] -> (Int -> m a) -> m a
withN defN [] f               = f defN
withN _    ps f               = g ps 0
  where
    g [] i                    = f i
    g (x:xs) i
      | x >= 48 && x <= 57    = g xs $! i * 256 - 48 + fromIntegral x
      | otherwise             = error $ "CSI: INVALID NUMBER " ++ show x

withNM :: Monad m => Int -> Int -> [Word8] -> (Int -> Int -> m a) -> m a
withNM defN defM []      f = f defN defM
withNM defN defM [59]    f = f defN defM
withNM defN defM (59:ps) f = withN defM ps (f defN)
withNM defN defM     ps  f = g ps 0
  where
    g [] i                 = fail "CSI: INVALID NUMBER"
    g (x:xs) i
      | x == 59            = withN defM xs (f i)
      | x >= 48 && x <= 57 = g xs $! i * 256 - 48 + fromIntegral x
      | otherwise          = fail "CSI: INVALID NUMBER"

withNumbers :: (Monad m, Integral n, Num n) => [Word8] -> ([n] -> m a) -> m a
withNumbers xs f = numbers' xs 0 >>= f
  where
    numbers' [] i
      = pure [i]
    numbers' (x:xs) i
      | x >= 48 && x <= 57 = numbers' xs (i * 256 - 48 + fromIntegral x)
      | x == 59            = (i:) <$> numbers' xs 0
      | otherwise          = fail ""

decodeUtf8Sequence :: MonadInput m => Word8 -> m Char
decodeUtf8Sequence x
  | x1                 < 0b10000000 = seq1
  | x1 .&. 0b11100000 == 0b11000000 = seq2
  | x1 .&. 0b11110000 == 0b11100000 = seq3
  | x1 .&. 0b11111000 == 0b11110000 = seq4
  | otherwise                       = reject
  where
    x1      = fromIntegral x :: Int
    reject  = pure '�'
    char c  = pure $ toEnum $ fromIntegral c
    seq1    = char x1
    seq2    = withNext $ \x2-> char $
      ((x1 .&. 0b00011111) `unsafeShiftL` 6) + x2
    seq3    = withNext $ \x2-> withNext $ \x3-> char $
      ((x1 .&. 0b00001111) `unsafeShiftL` 12) + (x2 `unsafeShiftL` 6) + x3
    seq4    = withNext $ \x2-> withNext $ \x3-> withNext $ \x4-> char $
      ((x1 .&. 0b00000111) `unsafeShiftL` 18) + (x2 `unsafeShiftL` 12) + (x3 `unsafeShiftL` 6) + x4
    withNext f = getNext >>= \case
      x | x .&. 0b11000000 == 0b10000000 -> f (fromIntegral x .&. 0b00111111)
        | otherwise                      -> reject

isKeyDelete :: MonadInput m => T.Event -> m Bool
isKeyDelete (T.EvKey T.KDelete {} [])       = pure True
isKeyDelete (T.EvKey (T.KChar [] c) [])     = askModes >>= \ms-> pure (c == '\DEL' && T.modeVERASE ms /= Just c)
isKeyDelete _                               = pure False

isKeyBackspace :: MonadInput m => T.Event -> m Bool
isKeyBackspace (T.EvKey T.KBackspace {} []) = pure True
isKeyBackspace (T.EvKey (T.KChar [] c) [])  = askModes >>= \ms-> pure (T.modeVERASE ms == Just c)
isKeyBackspace _                            = pure False
