module ThreeManChess.Engine.Piece where

import Data.Data
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.FigType

data Piece = Piece {figType :: FigType, color :: Color} deriving (Eq, Show, Read)
