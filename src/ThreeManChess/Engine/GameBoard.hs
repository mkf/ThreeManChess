module ThreeManChess.Engine.GameBoard where

import ThreeManChess.Engine.Pos
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

start :: GameBoard
start (Pos SecondOuter (File c _)) = Just Figure {figType = InwardPawn, color = c}
start (Pos MostOuter (File c s)) = Just Figure { color = c, figType = segmEightStarting s}
start _ = Nothing
