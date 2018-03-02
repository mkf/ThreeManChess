{-# Language DeriveDataTypeable #-}
module ThreeManChess.Engine.PieceType where

import Data.Data

data PieceType = Pawn | Rook | Knight | Bishop | King | Queen deriving (Eq, Show, Read, Data, Typeable)
