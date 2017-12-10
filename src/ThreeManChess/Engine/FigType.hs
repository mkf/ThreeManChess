{-# Language DeriveDataTypeable #-}
module ThreeManChess.Engine.FigType where

import Data.Data
import Data.Set (Set)

data FigType = Pawn | Rook | Knight | Bishop | King | Queen deriving (Eq, Show, Data, Typeable)
figTypeSet = Set.fromList (Pawn, Rook, Knight, Bishop, King, Queen)
