{-# Language DeriveDataTypeable #-}
module ThreeManChess.Engine.Figure where

import Data.Data
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.FigType
import ThreeManChess.Engine.Piece

data Figure = Pawn {figType :: FigType.Pawn, color :: Color.Color, crossedCenter :: Boolean} |
              Rook {figType :: FigType.Rook, color :: Color.Color} |
              Knight {figType :: FigType.Knight, color :: Color.Color} |
              Bishop {figType :: FigType.Bishop, color :: Color.Color} |
              King {figType :: FigType.King, color :: Color.Color} |
              Queen {figType :: FigType.Queen, color :: Color.Color} deriving (Piece, Eq, Show, Data, Typeable)
