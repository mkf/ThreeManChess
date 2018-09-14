module ThreeManChess.Engine.StartGameBoardTest where

import Data.Maybe
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
-- import Test.QuickCheck
import ThreeManChess.Engine.GameBoard
import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.Possibilities
import ThreeManChess.Engine.PosIterator
import ThreeManChess.Engine.Figure
import ThreeManChess.Engine.FigType
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.Move
import ThreeManChess.Engine.GameState


smOne :: StateMove
smOne = StateMove{move=(MkInwardPawnMove (Walk Forward), (1, 0)),
                  before=newGame}

whiteKingTest :: Assertion
whiteKingTest = [(0, 0+kfm)] @=? whereIsFig (Figure King White) startBoard

figTypeAndColorOKTest :: Assertion
figTypeAndColorOKTest = True @=? (checkIfFigTypeOK smOne && checkIfFigColorOK smOne)

firstAfterTest :: Assertion
firstAfterTest = True @=? isJust (afterWOblahblah smOne)

theAFileA :: SegmentEight
theAFileA = 0

justRight :: Either a b -> b
justRight (Right a) = a
justRight _ = error "yo"

a2a3Test :: Assertion
a2a3Test = [(MkInwardPawnMove (Walk Forward), (1, 0+theAFileA))] @=?
           justRight <$>
           boundMovesFromToWith
           InwardPawn
           (1, 0+theAFileA)
           (2, 0+theAFileA)
           White

noZeroDiagonalMoves :: Assertion
noZeroDiagonalMoves = fmap (const (Nothing,Nothing)) allPos @=? fmap (\x -> fromToDiagonals x x) allPos

noZeroFilewiseMoves :: Assertion
noZeroFilewiseMoves = fmap (const Nothing) allFilesFromZero @=? fmap (\x -> fromToFiles (x,x)) allFilesFromZero

tests :: [Test.Framework.Test]
tests = [testCase "whiteKingTest" whiteKingTest,
         testCase "figTypeAndColorOKTest" figTypeAndColorOKTest,
         testCase "a2a3Test" a2a3Test,
         testCase "noZeroDiagonalMoves" noZeroDiagonalMoves,
         testCase "noZeroFilewiseMoves" noZeroFilewiseMoves,
         testCase "firstAfterTest" firstAfterTest]
