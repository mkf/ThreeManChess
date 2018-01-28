-- {-# Language DeriveDataTypeable #-}
module ThreeManChess.Engine.FigType where

-- import Data.Data
-- import Data.Set as Set

data FigType = Pawn | Rook | Knight | Bishop | King | Queen deriving (Eq, Show, Read) --, Data, Typeable)
-- figTypeSet :: Set.Set FigType
-- figTypeSet = Set.fromList [Pawn, Rook, Knight, Bishop, King, Queen]
figTypes :: [FigType]
figTypes = [Pawn,Rook,Knight,Bishop,King,Queen]
