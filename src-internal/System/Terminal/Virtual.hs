module System.Terminal.Virtual where

import           Control.Monad.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import qualified Data.ByteString   as BS
import qualified Data.Text         as T

import           System.Terminal.MonadInput
import           System.Terminal.MonadScreen (EraseMode (..))
import           System.Terminal.Terminal

data VirtualTerminal
    = VirtualTerminal
    { virtualSettings          :: VirtualTerminalSettings
    , virtualCursor            :: TVar (Row, Col)
    , virtualWindow            :: TVar [String]
    , virtualAutoWrap          :: TVar Bool
    }

data VirtualTerminalSettings
    = VirtualTerminalSettings
    { virtualType              :: BS.ByteString
    , virtualWindowSize        :: STM (Rows,Cols)
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
    termGetCursorPosition = atomically . readTVar . virtualCursor

withVirtualTerminal :: (MonadIO m) => VirtualTerminalSettings -> (VirtualTerminal -> m a) -> m a
withVirtualTerminal settings handler = do
    size <- liftIO $ atomically $ virtualWindowSize settings
    term <- liftIO $ atomically $ VirtualTerminal settings
        <$> newTVar (0,0)
        <*> newTVar (replicate (fst size) (replicate (snd size) ' '))
        <*> newTVar True
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
    MoveCursorLeft i              -> moveCursorHorizontal t (negate i)
    MoveCursorRight i             -> moveCursorHorizontal t i
    ShowCursor                    -> pure ()
    HideCursor                    -> pure ()
    SaveCursor                    -> pure ()
    RestoreCursor                 -> pure ()
    GetCursorPosition             -> getCursorPosition t
    SetCursorPosition pos         -> setCursorPosition t pos
    SetCursorVertical r           -> setCursorVertical t r
    SetCursorHorizontal c         -> setCursorHorizontal t c
    InsertChars i                 -> insertChars       t i
    DeleteChars i                 -> deleteChars       t i
    EraseChars  i                 -> eraseChars        t i
    InsertLines i                 -> insertLines       t i
    DeleteLines i                 -> deleteLines       t i
    EraseInLine m                 -> eraseInLine       t m
    EraseInDisplay m              -> eraseInDisplay    t m
    SetAutoWrap b                 -> setAutoWrap       t b
{-
    SetAlternateScreenBuffer Bool
-}

scrollDown :: Cols -> [String] -> [String]
scrollDown width window =
    drop 1 window ++ [replicate width ' ']

putLn :: VirtualTerminal -> STM ()
putLn t = do
    (h,w)  <- virtualWindowSize (virtualSettings t)
    (r,_)  <- readTVar (virtualCursor t)
    window <- readTVar (virtualWindow t)
    if r + 1 == h
        then do
            writeTVar (virtualCursor t) (r, 0)
            writeTVar (virtualWindow t) (scrollDown w window)
        else do
            writeTVar (virtualCursor t) (r + 1, 0)

putString :: VirtualTerminal -> String -> STM ()
putString t s = do
    (h,w)    <- virtualWindowSize (virtualSettings t)
    autoWrap <- readTVar (virtualAutoWrap t)
    (r,c)    <- readTVar (virtualCursor t)
    wndw     <- readTVar (virtualWindow t)
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
            writeTVar (virtualCursor t) (min r' (h - 1), c')
        else do
            let (r', c') = (r, min (w - 1) (c + length s))
            writeTVar (virtualCursor t) (r', c')

moveCursorHorizontal :: VirtualTerminal -> Cols -> STM ()
moveCursorHorizontal t i = do
    (_,w)    <- virtualWindowSize (virtualSettings t)
    (r,c)    <- readTVar (virtualCursor t)
    writeTVar (virtualCursor t) (r, max 0 $ min (w - 1) $ c + i)

moveCursorVertical :: VirtualTerminal -> Cols -> STM ()
moveCursorVertical t i = do
    (h,_)    <- virtualWindowSize (virtualSettings t)
    (r,c)    <- readTVar (virtualCursor t)
    writeTVar (virtualCursor t) (max 0 $ min (h - 1) $ r + i, c)

getCursorPosition :: VirtualTerminal -> STM ()
getCursorPosition _ = pure ()

setCursorPosition :: VirtualTerminal -> (Row, Col) -> STM ()
setCursorPosition t (r,c) = do
    (h,w) <- virtualWindowSize (virtualSettings t)
    writeTVar (virtualCursor t) (max 0 (min (h - 1) r), max 0 (min (w - 1) c))

setCursorVertical :: VirtualTerminal -> Row -> STM ()
setCursorVertical t r = do
    (h,_) <- virtualWindowSize (virtualSettings t)
    (_,c) <- readTVar (virtualCursor t)
    writeTVar (virtualCursor t) (max 0 (min (h - 1) r), c)

setCursorHorizontal :: VirtualTerminal -> Col -> STM ()
setCursorHorizontal t c = do
    (_,w) <- virtualWindowSize (virtualSettings t)
    (r,_) <- readTVar (virtualCursor t)
    writeTVar (virtualCursor t) (r, max 0 (min (w - 1) c))

insertChars :: VirtualTerminal -> Int -> STM ()
insertChars t i = do
    (_,w) <- virtualWindowSize (virtualSettings t)
    (r,c) <- readTVar (virtualCursor t)
    wndw  <- readTVar (virtualWindow t)
    let l  = wndw !! r
        w1 = take r wndw
        w2 = [take w $ take c l <> replicate i ' ' <> drop c l]
        w3 = drop (r + 1) wndw
    writeTVar (virtualWindow t) (w1 <> w2 <> w3)

deleteChars :: VirtualTerminal -> Int -> STM ()
deleteChars t i = do
    (r,c) <- readTVar (virtualCursor t)
    wndw  <- readTVar (virtualWindow t)
    let l  = wndw !! r
        w1 = take r wndw
        w2 = [take c l <> drop (c + i) l <> replicate i ' ']
        w3 = drop (r + 1) wndw
    writeTVar (virtualWindow t) (w1 <> w2 <> w3)

eraseChars :: VirtualTerminal -> Int -> STM ()
eraseChars t i = do
    (r,c) <- readTVar (virtualCursor t)
    wndw  <- readTVar (virtualWindow t)
    let l  = wndw !! r
        w1 = take r wndw
        w2 = [take c l <> replicate i ' ' <> drop (c + i) l]
        w3 = drop (r + 1) wndw
    writeTVar (virtualWindow t) (w1 <> w2 <> w3)

insertLines :: VirtualTerminal -> Int -> STM ()
insertLines t i = do
    (h,w) <- virtualWindowSize (virtualSettings t)
    (r,_) <- readTVar (virtualCursor t)
    wndw  <- readTVar (virtualWindow t)
    let w1 = take r wndw
        w2 = replicate i (replicate w ' ')
        w3 = take (h - r - i) $ drop r wndw
    writeTVar (virtualWindow t) (w1 <> w2 <> w3)

deleteLines :: VirtualTerminal -> Int -> STM ()
deleteLines t i = do
    (h,w) <- virtualWindowSize (virtualSettings t)
    (r,_) <- readTVar (virtualCursor t)
    wndw  <- readTVar (virtualWindow t)
    let w1 = take r wndw
        w2 = take (h - r - i) $ drop r wndw
        w3 = replicate i (replicate w ' ')
    writeTVar (virtualWindow t) (w1 <> w2 <> w3)

eraseInLine :: VirtualTerminal -> EraseMode -> STM ()
eraseInLine t m = do
    (_,w) <- virtualWindowSize (virtualSettings t)
    (r,c) <- readTVar (virtualCursor t)
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
    (h,w) <- virtualWindowSize (virtualSettings t)
    (r,_) <- readTVar (virtualCursor t)
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
setAutoWrap t x = do
    writeTVar (virtualAutoWrap t) x
