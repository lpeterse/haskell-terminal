module System.Terminal.Virtual where

import           Control.Monad.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import qualified Data.ByteString   as BS
import qualified Data.Text         as T

import           System.Terminal.MonadInput
import           System.Terminal.MonadScreen (Size (..), Position (..), EraseMode (..))
import           System.Terminal.Terminal

data VirtualTerminal
    = VirtualTerminal
    { virtualSettings              :: VirtualTerminalSettings
    , virtualCursor                :: TVar Position
    , virtualWindow                :: TVar [String]
    , virtualAutoWrap              :: TVar Bool
    , virtualAlternateScreenBuffer :: TVar Bool
    }

data VirtualTerminalSettings
    = VirtualTerminalSettings
    { virtualType              :: BS.ByteString
    , virtualWindowSize        :: STM Size
    , virtualEvent             :: STM Event
    , virtualInterrupt         :: STM Interrupt
    }

instance Terminal VirtualTerminal where
    termType              = virtualType      . virtualSettings
    termEvent             = virtualEvent     . virtualSettings
    termInterrupt         = virtualInterrupt . virtualSettings
    termCommand t c       = atomically (command t c)
    termFlush _           = pure ()
    termGetWindowSize     = atomically . virtualWindowSize . virtualSettings
    termGetCursorPosition = readTVarIO . virtualCursor

withVirtualTerminal :: (MonadIO m) => VirtualTerminalSettings -> (VirtualTerminal -> m a) -> m a
withVirtualTerminal settings handler = do
    size <- liftIO $ atomically $ virtualWindowSize settings
    term <- liftIO $ atomically $ VirtualTerminal settings
        <$> newTVar (Position 0 0)
        <*> newTVar (replicate (height size) (replicate (width size) ' '))
        <*> newTVar True
        <*> newTVar False
    handler term

command :: VirtualTerminal -> Command -> STM ()
command t = \case
    PutLn                         -> putLn t
    PutText s                     -> putString t (T.unpack s)
    SetAttribute _                -> pure ()
    ResetAttribute _              -> pure ()
    ResetAttributes               -> pure ()
    MoveCursorUp i                -> moveCursorVertical   t (negate i)
    MoveCursorDown i              -> moveCursorVertical   t i
    MoveCursorForward i           -> moveCursorHorizontal t i
    MoveCursorBackward i          -> moveCursorHorizontal t (negate i)
    ShowCursor                    -> pure ()
    HideCursor                    -> pure ()
    SaveCursor                    -> pure ()
    RestoreCursor                 -> pure ()
    GetCursorPosition             -> getCursorPosition t
    SetCursorPosition pos         -> setCursorPosition t pos
    SetCursorRow r                -> setCursorRow t r
    SetCursorColumn c             -> setCursorColumn t c
    InsertChars i                 -> insertChars       t i
    DeleteChars i                 -> deleteChars       t i
    EraseChars  i                 -> eraseChars        t i
    InsertLines i                 -> insertLines       t i
    DeleteLines i                 -> deleteLines       t i
    EraseInLine m                 -> eraseInLine       t m
    EraseInDisplay m              -> eraseInDisplay    t m
    SetAutoWrap b                 -> setAutoWrap       t b
    SetAlternateScreenBuffer b    -> setAlternateScreenBuffer t b

scrollDown :: Int -> [String] -> [String]
scrollDown w window =
    drop 1 window ++ [replicate w ' ']

putLn :: VirtualTerminal -> STM ()
putLn t = do
    Size h w <- virtualWindowSize (virtualSettings t)
    Position r _ <- readTVar (virtualCursor t)
    window <- readTVar (virtualWindow t)
    if r + 1 == h
        then do
            writeTVar (virtualCursor t) $ Position r 0
            writeTVar (virtualWindow t) (scrollDown w window)
        else do
            writeTVar (virtualCursor t) $ Position (r + 1) 0

putString :: VirtualTerminal -> String -> STM ()
putString t s = do
    Size h w <- virtualWindowSize (virtualSettings t)
    autoWrap <- readTVar (virtualAutoWrap t)
    Position r c <- readTVar (virtualCursor t)
    wndw <- readTVar (virtualWindow t)
    let cl = w - c -- space in cursor line
        f "" ls     = ls
        f x  []     = let k = (take w x) in (k <> replicate (w - length k) ' ') : f (drop w x) []
        f x  (l:ls) = let k = (take w x) in (k <> drop (length k) l)            : f (drop w x) ls
        w1 = take r wndw
        w2 = [take c l <> k <> drop (c + length k) l]
            where
                k = take cl s
                l = wndw !! r
        w3  | autoWrap  = f (drop cl s) $ drop (r + 1) wndw
            | otherwise =                 drop (r + 1) wndw
        w4 = w1 <> w2 <> w3
    writeTVar (virtualWindow t) (reverse $ take h $ reverse w4)
    if autoWrap
        then do
            let (r',c') = quotRem (r * w + c + length s) w
            writeTVar (virtualCursor t) $ Position (min r' (h - 1)) c'
        else do
            let (r', c') = (r, min (w - 1) (c + length s))
            writeTVar (virtualCursor t) $ Position r' c'

moveCursorHorizontal :: VirtualTerminal -> Int -> STM ()
moveCursorHorizontal t i = do
    Size _ w <- virtualWindowSize (virtualSettings t)
    Position r c <- readTVar (virtualCursor t)
    writeTVar (virtualCursor t) $ Position r (max 0 $ min (w - 1) $ c + i)

moveCursorVertical :: VirtualTerminal -> Int -> STM ()
moveCursorVertical t i = do
    Size h _ <- virtualWindowSize (virtualSettings t)
    Position r c <- readTVar (virtualCursor t)
    writeTVar (virtualCursor t) $ Position (max 0 $ min (h - 1) $ r + i) c

getCursorPosition :: VirtualTerminal -> STM ()
getCursorPosition _ = pure ()

setCursorPosition :: VirtualTerminal -> Position -> STM ()
setCursorPosition t (Position r c) = do
    Size h w <- virtualWindowSize (virtualSettings t)
    writeTVar (virtualCursor t) $ Position (max 0 (min (h - 1) r)) (max 0 (min (w - 1) c))

setCursorRow :: VirtualTerminal -> Int -> STM ()
setCursorRow t r = do
    Size h _ <- virtualWindowSize (virtualSettings t)
    Position _ c <- readTVar (virtualCursor t)
    writeTVar (virtualCursor t) $ Position (max 0 (min (h - 1) r)) c

setCursorColumn :: VirtualTerminal -> Int -> STM ()
setCursorColumn t c = do
    Size _ w <- virtualWindowSize (virtualSettings t)
    Position r _ <- readTVar (virtualCursor t)
    writeTVar (virtualCursor t) $ Position r (max 0 (min (w - 1) c))

insertChars :: VirtualTerminal -> Int -> STM ()
insertChars t i = do
    Size _ w <- virtualWindowSize (virtualSettings t)
    Position r c <- readTVar (virtualCursor t)
    wndw  <- readTVar (virtualWindow t)
    let l  = wndw !! r
        w1 = take r wndw
        w2 = [take w $ take c l <> replicate i ' ' <> drop c l]
        w3 = drop (r + 1) wndw
    writeTVar (virtualWindow t) (w1 <> w2 <> w3)

deleteChars :: VirtualTerminal -> Int -> STM ()
deleteChars t i = do
    Position r c <- readTVar (virtualCursor t)
    wndw  <- readTVar (virtualWindow t)
    let l  = wndw !! r
        w1 = take r wndw
        w2 = [take c l <> drop (c + i) l <> replicate i ' ']
        w3 = drop (r + 1) wndw
    writeTVar (virtualWindow t) (w1 <> w2 <> w3)

eraseChars :: VirtualTerminal -> Int -> STM ()
eraseChars t i = do
    Position r c <- readTVar (virtualCursor t)
    wndw  <- readTVar (virtualWindow t)
    let l  = wndw !! r
        w1 = take r wndw
        w2 = [take c l <> replicate i ' ' <> drop (c + i) l]
        w3 = drop (r + 1) wndw
    writeTVar (virtualWindow t) (w1 <> w2 <> w3)

insertLines :: VirtualTerminal -> Int -> STM ()
insertLines t i = do
    Size h w <- virtualWindowSize (virtualSettings t)
    Position r _ <- readTVar (virtualCursor t)
    wndw  <- readTVar (virtualWindow t)
    let w1 = take r wndw
        w2 = replicate i (replicate w ' ')
        w3 = take (h - r - i) $ drop r wndw
    writeTVar (virtualWindow t) (w1 <> w2 <> w3)

deleteLines :: VirtualTerminal -> Int -> STM ()
deleteLines t i = do
    Size h w <- virtualWindowSize (virtualSettings t)
    Position r _ <- readTVar (virtualCursor t)
    wndw  <- readTVar (virtualWindow t)
    let w1 = take r wndw
        w2 = take (h - r - i) $ drop r wndw
        w3 = replicate i (replicate w ' ')
    writeTVar (virtualWindow t) (w1 <> w2 <> w3)

eraseInLine :: VirtualTerminal -> EraseMode -> STM ()
eraseInLine t m = do
    Size _ w <- virtualWindowSize (virtualSettings t)
    Position r c <- readTVar (virtualCursor t)
    wndw  <- readTVar (virtualWindow t)
    let l  = wndw !! r
        w1 = take r wndw
        w2 = case m of
                EraseBackward -> [replicate (c + 1) ' ' <> drop (c + 1) l]
                EraseForward  -> [take c l <> replicate (w - c) ' ']
                EraseAll      -> [replicate w ' ']
        w3 = drop (r + 1) wndw
    writeTVar (virtualWindow t) (w1 <> w2 <> w3)

eraseInDisplay :: VirtualTerminal -> EraseMode -> STM ()
eraseInDisplay t m = do
    Size h w <- virtualWindowSize (virtualSettings t)
    Position r _ <- readTVar (virtualCursor t)
    wndw  <- readTVar (virtualWindow t)
    let w1  = take r wndw
        w1E = replicate r (replicate w ' ') 
        w2  = [wndw !! r]
        w2E = [replicate w ' ']
        w3  = drop (r + 1) wndw
        w3E = replicate (h - r - 1) (replicate w ' ')
    writeTVar (virtualWindow t) $ case m of
        EraseBackward -> w1E <> w2  <> w3
        EraseForward  -> w1  <> w2  <> w3E
        EraseAll      -> w1E <> w2E <> w3E

setAutoWrap :: VirtualTerminal -> Bool -> STM ()
setAutoWrap t b = do
    writeTVar (virtualAutoWrap t) b

setAlternateScreenBuffer :: VirtualTerminal -> Bool -> STM ()
setAlternateScreenBuffer t b = do
    writeTVar (virtualAlternateScreenBuffer t) b
