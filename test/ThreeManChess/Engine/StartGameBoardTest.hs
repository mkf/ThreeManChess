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

whiteKingTest :: Assertion
whiteKingTest = [(MostOuter, File White kfm)] @=? whereIsFig (Figure King White) startBoard

tests :: [Test.Framework.Test]
tests = [testCase "whiteKingTest" whiteKingTest]
