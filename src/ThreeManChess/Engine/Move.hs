{-# LANGUAGE TypeFamilies, DataKinds, ExistentialQuantification, GADTs, KindSignatures #-}

module ThreeManChess.Engine.Move where

import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.Possibilities
import ThreeManChess.Engine.FigType

data (Vec a) => BoundVec a = BoundVec a Pos

data KingMove = AloneDiagonally DiagonalDirection | AloneRankwise RankwiseDirection | AloneFilewise FilewiseDirection | NotAlone Castling
data JustPawnMove = Forward | Capturing FilewiseDirection
-- data Promotion = forall a . PromotionDesire a => Promotion FigType
-- data Promotion where
--   Promotion :: PromotionDesire a => a -> Promotion
type PossiblyPromotedPawnMove = (JustPawnMove, Maybe Promotion)

type family Move (f :: FigType) where
  Move 'Queen = LinearVecC
  Move 'King = KingMove
  Move 'Rook = StraightVecC
  Move 'Bishop = LinearVec DiagonalDirection
  Move 'Knight = KnightVec
  Move 'InwardPawn = JustPawnMove
  Move 'OutwardPawn = PossiblyPromotedPawnMove
