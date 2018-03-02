{-# Language DeriveDataTypeable #-}
module ThreeManChess.Engine.Figure where

import Data.Data
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.FigType
-- import ThreeManChess.Engine.Piece

-- data Figure = Pawn {color :: Color, crossedCenter :: Bool} |
--               Rook {color :: Color} |
--               Knight {color :: Color} |
--               Bishop {color :: Color} |
--               King {color :: Color} |
--               Queen {color :: Color} deriving (--Piece,
--   Eq, Show,-- Data,
--   Typeable--, FigType
--   )

-- data Figure = Figure {figType :: FigType, color :: Color} deriving (Eq, Show, Read, Data, Typeable)
data Figure = Figure FigType Color deriving (Eq, Show, Read, Data, Typeable)

-- data Figure a where
--   Pawn :: {color :: Color, crossedCenter :: Bool} -> Figure FigType.FigType.Pawn
-- figType :: Figure -> FigType.FigType
-- figType (ThreeManChess.Engine.Figure.Pawn _ _) = FigType.Pawn
-- figType (ThreeManChess.Engine.Figure.Rook _) = FigType.Pawn
-- figType (ThreeManChess.Engine.Figure.Knight _) = FigType.Knight
-- figType (ThreeManChess.Engine.Figure.Bishop _) = FigType.Bishop
-- figType (ThreeManChess.Engine.Figure.King _) = FigType.King
-- figType (ThreeManChess.Engine.Figure.Queen _) = FigType.Queen
