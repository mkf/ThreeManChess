module ThreeManChess.Engine.StartGameBoardTest where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
-- import Test.QuickCheck
import ThreeManChess.Engine.GameBoard
import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.Figure
import ThreeManChess.Engine.FigType
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.Move
import ThreeManChess.Engine.GameState

whiteKingTest :: Assertion
whiteKingTest = [(MostOuter, File White kfm)] @=? whereIsFig (Figure King White) startBoard

figTypeAndColorOKTest :: Assertion
figTypeAndColorOKTest =
  let sm = StateMove{move=(MkInwardPawnMove (Walk Forward), (SecondOuter, File White (SegmentEight (SegmentQuarter FirstHalf FirstHalf) FirstHalf))),
                     before=newGame} in
    True @=? (checkIfFigTypeOK sm && checkIfFigColorOK sm)

tests :: [Test.Framework.Test]
tests = [testCase "whiteKingTest" whiteKingTest,
         testCase "figTypeAndColorOKTest" figTypeAndColorOKTest]
