module ThreeManChess.Engine.GameBoard where

import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.PosIterator (allPos)
import ThreeManChess.Engine.Board
import ThreeManChess.Engine.Figure
import ThreeManChess.Engine.FigType

type GameBoard = Board Figure
empty :: GameBoard
empty _ = Nothing
segmEightStarting :: SegmentEight -> FigType
segmEightStarting (SegmentEight (SegmentQuarter x y) z)
  | x==y && y==z = Rook
  | x==y = Knight
  | x==z = Bishop
  | x==FirstHalf && y==SecondHalf && z==SecondHalf = Queen
  | x==SecondHalf && y==FirstHalf && z==FirstHalf = King
  | otherwise = undefined

startBoard :: GameBoard
startBoard (SecondOuter, File c _) = Just $ Figure InwardPawn c
startBoard (MostOuter, File c s) = Just $ Figure (segmEightStarting s) c
startBoard _ = Nothing

whereIsFig :: Figure -> GameBoard -> [Pos]
whereIsFig what board = filter (maybe False (what ==) . board) allPos
