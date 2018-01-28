module ThreeManChess.Engine.GameBoard where

import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.Board
import ThreeManChess.Engine.Figure

type GameBoard = Board Figure
empty :: GameBoard
start :: GameBoard
start (Pos SecondOuter (File c _)) = Pawn {color = c, crossedCenter = False}
start (Pos MostOuter (File c s)) =
  case s of (SegmentFile (SegmentQuarter x x) x) -> Rook {color = c}
            (SegmentFile (SegmentQuarter x x) _) -> Knight {color = c}
            (SegmentFile (SegmentQuarter x _) x) -> Bishop {color = c}
            (SegmentFile (SegmentQuarter w x) x) -> case w of FirstHalf -> Queen {color = c}
                                                              SecondHalf -> King {color = c}
start _ = Nothing
