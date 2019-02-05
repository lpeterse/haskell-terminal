module Spec.Virtual (tests) where

import           Control.Monad
import           Control.Concurrent.STM.TVar
import           Data.Monoid

import           Test.Tasty
import           Test.Tasty.HUnit

import           System.Terminal.MonadScreen (EraseMode (..))
import           System.Terminal.Terminal
import           System.Terminal.Virtual

tests :: TestTree
tests = testGroup "System.Terminal.Virtual"
    [ testGroup "withVirtualTerminal"
        [ testWithVirtualTerminal01
        ]
    , testGroup "PutLn"
        [ testPutLn01
        , testPutLn02
        , testPutLn03
        ]
    , testGroup "PutText"
        [ testPutText01
        , testPutText02
        , testPutText03
        , testPutText04
        ]
    , testGroup "MoveCursor*"
        [ testMoveCursor01
        , testMoveCursor02
        , testMoveCursor03
        , testMoveCursor04
        ]
    , testGroup "GetCursorPosition"
        [ testGetCursorPosition01
        ]
    , testGroup "SetCursorPosition"
        [ testSetCursorPosition01
        , testSetCursorPosition02
        , testSetCursorPosition03
        , testSetCursorPosition04
        , testSetCursorPosition05
        ]
    , testGroup "SetCursorVertical"
        [ testSetCursorVertical01
        ]
    , testGroup "SetCursorHorizontal" 
        [ testSetCursorHorizontal01
        ]
    , testGroup "InsertChars"
        [ testInsertChars01
        ]
    , testGroup "DeleteChars"
        [ testDeleteChars01
        ]
    , testGroup "EraseChars"
        [ testEraseChars01
        ]
    , testGroup "InsertLines"
        [ testInsertLines01
        ]
    , testGroup "DeleteLines"
        [ testDeleteLines01
        ]
    , testGroup "EraseInLine"
        [ testEraseInLine01
        , testEraseInLine02
        , testEraseInLine03
        ]
    , testGroup "EraseInDisplay"
        [ testEraseInDisplay01
        , testEraseInDisplay02
        , testEraseInDisplay03
        ]
    ]

testWithVirtualTerminal01 :: TestTree
testWithVirtualTerminal01 =
    testCase "shall yield empty window and cursor on home position" do
        t <- withVirtualTerminal settings pure
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (0,0)
        expWindow =
            [ "          "
            , "          "
            , "          " ]

testPutLn01 :: TestTree
testPutLn01 =
    testCase "shall set cursor to next line" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t PutLn
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,0)
        expWindow =
            [ "          "
            , "          "
            , "          " ]

testPutLn02 :: TestTree
testPutLn02 =
    testCase "shall not exceed bottom margin" do
        t <- withVirtualTerminal settings $ \t -> do
            replicateM_ 6 (termCommand t PutLn)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (2,0)
        expWindow =
            [ "          "
            , "          "
            , "          " ]

testPutLn03 :: TestTree
testPutLn03 =
    testCase "shall scroll when reaching bottom margin" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "ABC")
            termCommand t PutLn
            termCommand t (PutText "123")
            termCommand t PutLn
            termCommand t PutLn
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (2,0)
        expWindow =
            [ "123       "
            , "          "
            , "          " ]

testPutText01 :: TestTree
testPutText01 =
    testCase "shall put text in first line" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "ABC")
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (0,3)
        expWindow =
            [ "ABC       "
            , "          "
            , "          " ]

testPutText02 :: TestTree
testPutText02 =
    testCase "shall wrap text around (when autoWrap is on)" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (SetAutoWrap True)
            termCommand t (PutText "ABC")
            termCommand t (PutText "123456789")
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,2)
        expWindow =
            [ "ABC1234567"
            , "89        "
            , "          " ]

testPutText03 :: TestTree
testPutText03 =
    testCase "shall not wrap text around (when autoWrap is off)" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (SetAutoWrap False)
            termCommand t (PutText "ABC")
            termCommand t (PutText "123456789")
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (0,9)
        expWindow =
            [ "ABC1234567"
            , "          "
            , "          " ]

testPutText04 :: TestTree
testPutText04 =
    testCase "shall scroll when reaching bottom margin" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "1234567890ABCDEFGHIJabcdefghijX")
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (2,1)
        expWindow =
            [ "ABCDEFGHIJ"
            , "abcdefghij"
            , "X         " ]

testMoveCursor01 :: TestTree
testMoveCursor01 =
    testCase "shall move up" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "123456789012345")
            termCommand t (MoveCursorUp 1)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (0,5)
        expWindow =
            [ "1234567890"
            , "12345     "
            , "          " ]

testMoveCursor02 :: TestTree
testMoveCursor02 =
    testCase "shall move down" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "123456789012345")
            termCommand t (MoveCursorDown 1)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (2,5)
        expWindow =
            [ "1234567890"
            , "12345     "
            , "          " ]

testMoveCursor03 :: TestTree
testMoveCursor03 =
    testCase "shall move forward" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "123456789012345")
            termCommand t (MoveCursorRight 2)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,7)
        expWindow =
            [ "1234567890"
            , "12345     "
            , "          " ]

testMoveCursor04 :: TestTree
testMoveCursor04 =
    testCase "shall move backward" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "123456789012345")
            termCommand t (MoveCursorLeft 2)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,3)
        expWindow =
            [ "1234567890"
            , "12345     "
            , "          " ]

testGetCursorPosition01 :: TestTree
testGetCursorPosition01 =
    testCase "shall return cursor position" do
        pos <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "123456789012345")
            termGetCursorPosition t
        assertEqual "cursor" expCursor pos
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,5)

testSetCursorPosition01 :: TestTree
testSetCursorPosition01 =
    testCase "shall set cursor position" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "123456789012345")
            termCommand t (SetCursorPosition (2,8))
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (2,8)
        expWindow =
            [ "1234567890"
            , "12345     "
            , "          " ]

testSetCursorPosition02 :: TestTree
testSetCursorPosition02 =
    testCase "shall set cursor position (limit top margin)" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "123456789012345")
            termCommand t (SetCursorPosition (-1,8))
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (0,8)
        expWindow =
            [ "1234567890"
            , "12345     "
            , "          " ]

testSetCursorPosition03 :: TestTree
testSetCursorPosition03 =
    testCase "shall set cursor position (limit bottom margin)" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "123456789012345")
            termCommand t (SetCursorPosition (5,8))
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (2,8)
        expWindow =
            [ "1234567890"
            , "12345     "
            , "          " ]

testSetCursorPosition04 :: TestTree
testSetCursorPosition04 =
    testCase "shall set cursor position (limit left margin)" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "123456789012345")
            termCommand t (SetCursorPosition (1,-1))
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,0)
        expWindow =
            [ "1234567890"
            , "12345     "
            , "          " ]

testSetCursorPosition05 :: TestTree
testSetCursorPosition05 =
    testCase "shall set cursor position (limit right margin)" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "123456789012345")
            termCommand t (SetCursorPosition (1,11))
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,9)
        expWindow =
            [ "1234567890"
            , "12345     "
            , "          " ]

testSetCursorVertical01 :: TestTree
testSetCursorVertical01 =
    testCase "shall set vertical cursor position" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "123456789012345")
            termCommand t (SetCursorVertical 2)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (2,5)
        expWindow =
            [ "1234567890"
            , "12345     "
            , "          " ]

testSetCursorHorizontal01 :: TestTree
testSetCursorHorizontal01 =
    testCase "shall set horizontal cursor position" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "123456789012345")
            termCommand t (SetCursorHorizontal 8)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,8)
        expWindow =
            [ "1234567890"
            , "12345     "
            , "          " ]

testInsertChars01 :: TestTree
testInsertChars01 =
    testCase "shall insert space and shift out existing chars" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "1234567890")
            termCommand t (SetCursorPosition (0,4))
            termCommand t (InsertChars 3)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (0,4)
        expWindow =
            [ "1234   567"
            , "          "
            , "          " ]

testDeleteChars01 :: TestTree
testDeleteChars01 =
    testCase "shall shift chars from right and fill with whitespace" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "1234567890")
            termCommand t (SetCursorPosition (0,4))
            termCommand t (DeleteChars 3)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (0,4)
        expWindow =
            [ "1234890   "
            , "          "
            , "          " ]

testEraseChars01 :: TestTree
testEraseChars01 =
    testCase "shall override chars with spaces" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "1234567890")
            termCommand t (SetCursorPosition (0,4))
            termCommand t (EraseChars 3)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (0,4)
        expWindow =
            [ "1234   890"
            , "          "
            , "          " ]

testInsertLines01 :: TestTree
testInsertLines01 =
    testCase "shall shift lines cursor and below downwards" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "1234567890")
            termCommand t (PutText "ABCDEFGHIJ")
            termCommand t (PutText "QRSTUVWXYZ")
            termCommand t (SetCursorPosition (1,4))
            termCommand t (InsertLines 1)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (4,10)
            }
        expCursor = (1,4)
        expWindow =
            [ "1234567890"
            , "          "
            , "ABCDEFGHIJ"
            , "QRSTUVWXYZ" ]

testDeleteLines01 :: TestTree
testDeleteLines01 =
    testCase "shall shift chars from right and fill with whitespace" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "1234567890")
            termCommand t (PutText "ABCDEFGHIJ")
            termCommand t (PutText "QRSTUVWXYZ")
            termCommand t (SetCursorPosition (1,4))
            termCommand t (InsertLines 1)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (4,10)
            }
        expCursor = (1,4)
        expWindow =
            [ "1234567890"
            , "          "
            , "ABCDEFGHIJ"
            , "QRSTUVWXYZ" ]

testEraseInLine01 :: TestTree
testEraseInLine01 =
    testCase "shall erase left with EraseBackwards" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "1234567890")
            termCommand t (PutText "1234567890")
            termCommand t (PutText "1234567890")
            termCommand t (SetCursorPosition (1,4))
            termCommand t (EraseInLine EraseBackward)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,4)
        expWindow =
            [ "1234567890"
            , "     67890"
            , "1234567890" ]

testEraseInLine02 :: TestTree
testEraseInLine02 =
    testCase "shall erase right with EraseForward" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "1234567890")
            termCommand t (PutText "1234567890")
            termCommand t (PutText "1234567890")
            termCommand t (SetCursorPosition (1,4))
            termCommand t (EraseInLine EraseForward)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,4)
        expWindow =
            [ "1234567890"
            , "1234      "
            , "1234567890" ]

testEraseInLine03 :: TestTree
testEraseInLine03 =
    testCase "shall erase whole line with EraseAll" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "1234567890")
            termCommand t (PutText "1234567890")
            termCommand t (PutText "1234567890")
            termCommand t (SetCursorPosition (1,4))
            termCommand t (EraseInLine EraseAll)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,4)
        expWindow =
            [ "1234567890"
            , "          "
            , "1234567890" ]

testEraseInDisplay01 :: TestTree
testEraseInDisplay01 =
    testCase "shall erase left with EraseBackwards" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "1234567890")
            termCommand t (PutText "1234567890")
            termCommand t (PutText "1234567890")
            termCommand t (SetCursorPosition (1,4))
            termCommand t (EraseInDisplay EraseBackward)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,4)
        expWindow =
            [ "          "
            , "1234567890"
            , "1234567890" ]

testEraseInDisplay02 :: TestTree
testEraseInDisplay02 =
    testCase "shall erase right with EraseForward" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "1234567890")
            termCommand t (PutText "1234567890")
            termCommand t (PutText "1234567890")
            termCommand t (SetCursorPosition (1,4))
            termCommand t (EraseInDisplay EraseForward)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,4)
        expWindow =
            [ "1234567890"
            , "1234567890"
            , "          " ]

testEraseInDisplay03 :: TestTree
testEraseInDisplay03 =
    testCase "shall erase whole line with EraseAll" do
        t <- withVirtualTerminal settings $ \t -> do
            termCommand t (PutText "1234567890")
            termCommand t (PutText "1234567890")
            termCommand t (PutText "1234567890")
            termCommand t (SetCursorPosition (1,4))
            termCommand t (EraseInDisplay EraseAll)
            pure t
        assertEqual "window" expWindow =<< readTVarIO (virtualWindow t)
        assertEqual "cursor" expCursor =<< readTVarIO (virtualCursor t)
    where
        settings = defaultSettings
            { virtualWindowSize = pure (3,10)
            }
        expCursor = (1,4)
        expWindow =
            [ "          "
            , "          "
            , "          " ]
