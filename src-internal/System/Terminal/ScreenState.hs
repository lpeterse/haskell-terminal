module System.Terminal.ScreenState where

import System.Terminal.MonadInput

data ScreenState
    = ScreenState
    { ssAutoWrap          :: !Bool
    , ssAlternate         :: !Bool
    , ssWindowHeight      :: !Rows
    , ssWindowWidth       :: !Cols
    , ssWindowRow         :: !Row
    , ssWindowCol         :: !Col
    , ssCursorRow         :: !Row
    , ssCursorCol         :: !Col
    , ssCursorRowSaved    :: !Row
    , ssCursorColSaved    :: !Col
    , ssCursorRowSavedAbs :: !Row
    , ssCursorColSavedAbs :: !Col
    }

ssDefault :: ScreenState
ssDefault = ScreenState
    { ssAutoWrap          = True
    , ssAlternate         = False
    , ssWindowHeight      = 24
    , ssWindowWidth       = 80
    , ssWindowRow         = 0
    , ssWindowCol         = 0
    , ssCursorRow         = 0
    , ssCursorCol         = 0
    , ssCursorRowSaved    = 0
    , ssCursorColSaved    = 0
    , ssCursorRowSavedAbs = 0
    , ssCursorColSavedAbs = 0
    }

ssResetPositions :: (Rows, Cols) ->  (Row, Col) -> ScreenState -> ScreenState
ssResetPositions (height, width) (row,col) ss = ss 
    { ssWindowHeight   = height
    , ssWindowWidth    = width
    , ssWindowRow      = 0
    , ssWindowCol      = 0
    , ssCursorRow      = row
    , ssCursorCol      = col
    -- adjust saved cursor position by tracking deviation
    , ssCursorRowSaved = ssCursorRowSaved ss + (ssCursorRow ss - row)
    , ssCursorColSaved = ssCursorColSaved ss + (ssCursorCol ss - col)
    }

ssGetWindowSize :: ScreenState -> (Rows, Cols)
ssGetWindowSize ss =
    ( ssWindowHeight ss, ssWindowWidth ss )

ssSetWindowSize :: (Rows, Cols) -> ScreenState -> ScreenState
ssSetWindowSize (height, width) ss = ss
    { ssWindowHeight = height
    , ssWindowWidth  = width
    }

ssPutLn :: ScreenState -> ScreenState
ssPutLn ss = ss
    { ssWindowRow   = ssWindowRow ss + linesScrolled
    , ssCursorRow   = cursorRow'
    , ssCursorCol   = cursorCol'
    }
    where
        cursorRow'
            | ssAlternate ss = min (ssCursorRow ss + 1) (ssWindowRow ss + ssWindowHeight ss - 1)
            | otherwise      = ssCursorRow ss + 1
        cursorCol'           = ssWindowCol ss
        linesScrolled
            | ssAlternate ss = 0
            | otherwise      = cursorRow' - ssWindowRow ss - (ssWindowHeight ss - 1)

ssPutText :: Int -> ScreenState -> ScreenState
ssPutText i ss = ss
    { ssWindowRow = ssWindowRow ss + linesScrolled
    , ssCursorRow = cursorRow'
    , ssCursorCol = cursorCol'
    }
    where
        width         = ssWindowWidth ss
        height        = ssWindowHeight ss
        cursorRow     = ssCursorRow ss
        cursorCol     = ssCursorCol ss
        (cursorRow', cursorCol')
            | ssAutoWrap ss  = quotRem (width * cursorRow + cursorCol + i) width
            | otherwise      = (cursorRow, max (cursorCol + i) (width - 1))
        linesScrolled
            | ssAutoWrap ss  = 0
            | ssAlternate ss = 0
            | otherwise      = cursorRow' - cursorRow - (height - 1)

ssGetCursorPosition :: ScreenState -> (Row, Col)
ssGetCursorPosition ss =
    ( ssCursorRow ss - ssWindowRow ss
    , ssCursorCol ss - ssWindowCol ss
    )

ssSetCursorPosition :: (Row, Col) -> ScreenState -> ScreenState
ssSetCursorPosition (row, col) ss = ss
    { ssCursorRow = max (ssWindowRow ss) $ min (ssWindowWidth  ss - 1) row
    , ssCursorCol = max (ssWindowCol ss) $ min (ssWindowHeight ss - 1) col
    }

ssSaveCursor :: ScreenState -> ScreenState
ssSaveCursor ss = ss
    { ssCursorRowSaved = ssCursorRow ss - ssWindowRow ss
    , ssCursorColSaved = ssCursorCol ss - ssWindowCol ss
    }

ssRestoreCursor :: ScreenState -> ScreenState
ssRestoreCursor ss = ss
    { ssCursorRow = ssCursorRowSaved ss + ssWindowRow ss
    , ssCursorCol = ssCursorColSaved ss + ssWindowCol ss
    }

ssSaveCursorPosition :: ScreenState -> ScreenState
ssSaveCursorPosition ss = ss
    { ssCursorRowSavedAbs = ssCursorRow ss
    , ssCursorColSavedAbs = ssCursorCol ss
    }

ssLoadCursorPosition :: ScreenState -> (Row,Col)
ssLoadCursorPosition ss =
    ( ssCursorRowSavedAbs ss - ssWindowRow ss
    , ssCursorColSavedAbs ss - ssWindowCol ss
    )

ssMoveUp :: Rows -> ScreenState -> ScreenState
ssMoveUp i ss = ss
    { ssCursorRow = max (ssCursorRow ss - i) (ssWindowRow ss)
    }

ssMoveDown :: Rows -> ScreenState -> ScreenState
ssMoveDown i ss = ss
    { ssCursorRow = min (ssCursorRow ss + i) (ssWindowRow ss + ssWindowHeight ss - 1)
    }

ssMoveLeft :: Cols -> ScreenState -> ScreenState
ssMoveLeft i ss = ss
    { ssCursorCol = max (ssCursorCol ss - i) (ssWindowCol ss)
    }

ssMoveRight :: Cols -> ScreenState -> ScreenState
ssMoveRight i ss = ss
    { ssCursorCol = min (ssCursorCol ss + i) (ssWindowCol ss + ssWindowWidth ss - 1)
    }

ssSetAutoWrap :: Bool -> ScreenState -> ScreenState
ssSetAutoWrap x ss = ss
    { ssAutoWrap = x
    }

ssSetAlternateScreenBuffer :: Bool -> ScreenState -> ScreenState
ssSetAlternateScreenBuffer x ss = ss
    { ssAlternate = x
    }
