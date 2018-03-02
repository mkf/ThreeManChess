{-# Language DeriveDataTypeable #-}
module ThreeManChess.Engine.Piece where

import Data.Data
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.PieceType

-- data Piece = Piece {pieceType :: PieceType, color :: Color} deriving (Eq, Show, Read, Data, Typeable)
data Piece = Piece PieceType Color deriving (Eq, Show, Read, Data, Typeable)
