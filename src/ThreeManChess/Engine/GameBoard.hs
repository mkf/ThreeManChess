module ThreeManChess.Engine.GameBoard where

import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.PosIterator (allPos)
import ThreeManChess.Engine.Board
import ThreeManChess.Engine.Figure
import ThreeManChess.Engine.FigType

{-@ type GameBoard = Board Figure @-}
type GameBoard = Board Figure
type GameBoardSingleChange = BoardSingleChange Figure
empty :: GameBoard
empty _ = Nothing
{-@ segmEightStarting :: SegmentEight -> FigType @-}
segmEightStarting :: SegmentEight -> FigType
segmEightStarting n -- (SegmentEight (SegmentQuarter x y) z)
  | n==0 || n==7 = Rook --x==y && y==z = Rook
  | n==1 || n==6 = Knight -- x==y
  | n==2 || n==5 = Bishop -- x==z
  | n==3 = Queen -- x==FirstHalf && y==SecondHalf && z==SecondHalf = Queen
  | n==4 = King  -- x==SecondHalf && y==FirstHalf && z==FirstHalf = King

{-@ startBoard :: GameBoard @-}
startBoard :: GameBoard
startBoard (1, f) = Just $ Figure InwardPawn (segmColor f)
startBoard (0, f) = Just $ Figure (segmEightStarting $ segmFile f) (segmColor f)
startBoard _ = Nothing

whereIsMaybeFig :: Maybe Figure -> GameBoard -> [Pos]
whereIsMaybeFig what board = filter ((what ==) . board) allPos
whereIsFig :: Figure -> GameBoard -> [Pos]
-- whereIsFig what board = filter (maybe False (what ==) . board) allPos
whereIsFig what = whereIsMaybeFig (Just what)
