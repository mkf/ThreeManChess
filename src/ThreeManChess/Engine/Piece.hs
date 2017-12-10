{-# Language DeriveDataTypeable #-}
module ThreeManChess.Engine.Piece where

import Data.Data
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.FigType

data Piece = Piece {figType :: FigType.FigType, color :: Color.Color}
  deriving (Eq, Show, Data, Typeable)
