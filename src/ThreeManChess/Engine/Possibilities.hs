{-# Language DuplicateRecordFields,GADTs #-}
module ThreeManChess.Engine.Possibilities where

import Data.Data
import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.Figure

class --(Eq a , Read a, Show a) =>
  Reversable a where
  rever :: a -> a
class (Reversable a, Eq a-- , Ord a
      ) => LinearDirection a
data Orientation = Rankwise | Filewise deriving (Eq, Read, Show)
perpendicularTo :: Orientation -> Orientation
perpendicularTo Rankwise = Filewise
perpendicularTo Filewise = Rankwise
class (LinearDirection a) => StraightDirection a where
  orientation :: a -> Orientation
data RankwiseDirection = Inwards | Outwards -- deriving StraightDirection
instance Reversable RankwiseDirection where
  rever Inwards = Outwards
  rever Outwards = Inwards
instance StraightDirection RankwiseDirection where
  orientation _ = Rankwise
instance LinearDirection RankwiseDirection
instance Eq RankwiseDirection where
  Inwards == Inwards = True
  Outwards == Outwards = True
  Inwards == Outwards = False
  Outwards == Inwards = False
data FilewiseDirection = Pluswards | Minuswards -- deriving StraightDirection
instance Reversable FilewiseDirection where
  rever Pluswards = Minuswards
  rever Minuswards = Pluswards
instance StraightDirection FilewiseDirection where
  orientation _ = Filewise
instance LinearDirection FilewiseDirection
instance Eq FilewiseDirection where
  Pluswards == Pluswards = True
  Minuswards == Minuswards = True
  Pluswards == Minuswards = False
  Minuswards == Pluswards = False
data DiagonalDirection = DiagonalDirection RankwiseDirection FilewiseDirection -- deriving LinearDirection
instance LinearDirection DiagonalDirection
instance Eq DiagonalDirection where
  (DiagonalDirection a b) == (DiagonalDirection c d) = (a == c) && (b == d)
instance Reversable DiagonalDirection where
  rever (DiagonalDirection a b) = DiagonalDirection (rever a) (rever b)
data Count = Once | OnceMore Count deriving (Eq, Read, Show)
instance Ord Count where
  Once `compare` Once = EQ
  OnceMore a `compare` OnceMore b = a `compare` b
  Once `compare` OnceMore _ = LT
  OnceMore _ `compare` Once = GT
class (Eq a-- , Read a, Show a
      ) => Vec a where
  reverMaybe :: a -> Maybe a
class (Vec a, Reversable a) => ReversableVec a
-- instance (Reversable a) => Vec (ReversableVec a) where
--   reverMaybe x = Just $ rever x
data Castling = QueensideCastling | KingsideCastling -- deriving (Vec)
instance Eq Castling where
  QueensideCastling == QueensideCastling = True
  QueensideCastling == KingsideCastling = False
  KingsideCastling == QueensideCastling = False
  KingsideCastling == KingsideCastling = True
instance Vec Castling where
  reverMaybe _ = Nothing
data PawnJumpByTwo = PawnJumpByTwo --deriving (Vec)
instance Eq PawnJumpByTwo where
  PawnJumpByTwo == PawnJumpByTwo = True
instance Vec PawnJumpByTwo where
  reverMaybe PawnJumpByTwo = Nothing
data LinearVec a = LinearVec a Count --deriving (Ord)
-- instance (LinearDirection a) => Ord (LinearVec a)
instance (LinearDirection a) => ReversableVec (LinearVec a)
instance (LinearDirection a) => Reversable (LinearVec a) where
  rever (LinearVec d n) = (LinearVec (rever d) n)
instance (Eq a) => Eq (LinearVec a) where
  (LinearVec a c) == (LinearVec b d) = (a == b) && (c == d)
instance (LinearDirection a) => Vec (LinearVec a) where
  reverMaybe x = Just $ rever x
data KnightVec = KnightVec RankwiseDirection FilewiseDirection Orientation -- Orientation :: twice
-- instance Ord KnightVec
instance Eq KnightVec where
  (KnightVec r f o) == (KnightVec rr ff oo) = (r==rr)&&(f==ff)&&(o==oo)
instance ReversableVec KnightVec
instance Reversable KnightVec where
  rever (KnightVec r f orient) = (KnightVec (rever r) (rever f) orient)
instance Vec KnightVec where
  reverMaybe x = Just $ rever x

-- class Straigt

-- type StraightMove = LinearMove { direction :: StraightDirection }
-- type DiagonalMove = LinearMove { direction :: DiagonalDirection }
-- type RankwiseMove = StraightMove { direction :: RankwiseDirection }
-- type FilewiseMove = StraightMove { direction :: FilewiseDirection }
-- data RankInvolMove = RankwiseMove | DiagonalMove deriving (Eq, Ord, Read, Show, LinearMove)
-- type UnitLinearMove = LinearMove { count :: Once }
-- type UnitStraightMove = UnitLinearMove { direction :: StraightDirection }
-- type UnitDiagonalMove = UnitLinearMove { direction :: DiagonalDirection }
-- type UnitRankwiseMove = UnitStraightMove { direction :: RankwiseDirection }
-- type UnitFilewiseMove = UnitStraightMove { direction :: FilewiseDirection }
-- newtype UnitRankInvolMove = UnitRankwiseMove | UnitDiagonalMove
--                           deriving (Eq, Ord, Read, Show, UnitLinearMove, RankInvolMove)
-- headLinear :: LinearMove -> UnitLinearMove
-- headLinear LinearMove {direction=d} = UnitLinearMove {direction=d}
-- tailFilewise :: FilewiseMove -> Maybe UnitStraightMove
-- tailFilewise FilewiseMove {count=Once} = Nothing
-- tailFilewise FilewiseMove {direction=d, count=OnceMore c} = FilewiseMove {direction=d, count=c}
-- tailRankInvol :: LinearMove -> Maybe Either (Rank -> RankInvolMove) LinearMove
-- tailRankInvol RankInvolMove {count=Once} = Nothing
-- tailRankInvol RankwiseMove {direction=Inwards, count=OnceMore c} =
--   Just Left (\x -> RankwiseMove {direction =
--                                case x of MostInner -> Outwards
--                                          _ -> Inwards,
--                             count=c})
-- tailRankInvol DiagonalMove {direction=DiagonalDirection {rankwise=Inwards, filewise=f}, count=OnceMore c} =
--   Just Left (\x -> DiagonalMove {direction=
--                                case x of MostInner -> DiagonalDirection{
--                                            rankwise=Outwards, filewise=rever f}
--                                          _ -> DiagonalDirection{rankwise=Inwards, filewise=f},
--                             count=c})
-- tailRankInvol LinearMove {direction=d, count=OnceMore c} = Just Right LinearMove {direction=d, count=c}
-- units :: LinearMove -> Either (Rank -> [UnitRankInvolMove]) [UnitLinearMove]
-- units x = let
--   {t = tailRankInvol x;
--    h = headLinear x} in h :
--     case t of
--       Just e -> either (\left rank -> units $ left rank) units  e
--       Nothing -> []
-- unitsInvolRank :: LinearMove -> Rank -> [UnitLinearMove]
-- unitsInvolRank x = either id const $ units x

-- add :: Move -> Pos -> Maybe Pos
-- add KnightMove {rankwise=r, filewise=f, twice=t} x =
--   do { fl <- add UnitStraightMove {direction=case t of Rankwise -> r; Filewise -> f} x;
--        return add UnitStraightMove {direction=case t of Filewise -> r; Rankwise -> f} fl }
-- add QueensideCastling (Pos MostOuter (File c (SegmentEight (SegmentQuarter SecondHalf FirstHalf) FirstHalf))) =
--   Just Pos {rank=MostOuter, file=File{color=c,
--                                       colorSegmFile=SegmentEight{
--                                          segmentQuarter=SegmentQuarter {
--                                              half=FirstHalf, halfQuarter=SecondHalf}, quarterHalf=FirstHalf}}}
-- add QueensideCastling _ = undefined
-- add KingsideCastling (Pos MostOuter (File c (SegmentEight (SegmentQuarter SecondHalf FirstHalf) FirstHalf))) =
--   Just Pos {rank=MostOuter, file=File{color=c,
--                                       colorSegmFile=SegmentEight{
--                                          segmentQuarter=SegmentQuarter {
--                                              half=SecondHalf, halfQuarter=SecondHalf}, quarterHalf=FirstHalf}}}
-- add KingsideCastling _ = undefined
-- add UnitRankwiseMove {direction=Inwards, count=Once} (Pos MostInner file) = Just Pos {rank=MostInner,file=opposite file}
-- add UnitRankwiseMove {direction=Inwards, count=Once} (Pos rank file) = Just Pos {rank=inw rank,file=file}
-- add UnitRankwiseMove {direction=Outwards, count=Once} (Pos rank file) =
--   do { o <- out rank;
--        return Pos {rank=o,file=file} }
-- add UnitFilewiseMove {direction=Pluswards, count=Once} (Pos rank file) = Just Pos {rank=rank, file=plus file}
-- add UnitFilewiseMove {direction=Minuswards, count=Once} (Pos rank file) = Just Pos {rank=rank, file=minus file}
-- add UnitDiagonalMove {direction=(DiagonalDirection Inwards p), count=Once}
--                      (Pos MostInner (File c (SegmentEight q r))) =
--   Just Pos{
--   rank=MostInner,
--   file =
--       case p of
--         Pluswards ->
--           case q of
--             SegmentQuarter SecondHalf SecondHalf ->
--               File{color=prev c,
--                     colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter FirstHalf FirstHalf,quarterHalf=r}}
--             SegmentQuarter SecondHalf FirstHalf ->
--               File{color=next c,
--                    colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter SecondHalf SecondHalf, quarterHalf=r}}
--             SegmentQuarter FirstHalf SecondHalf ->
--               File{color=next c,
--                    colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter SecondHalf FirstHalf, quarterHalf=r}}
--             SegmentQuarter FirstHalf FirstHalf ->
--               File{color=next c,
--                    colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter FirstHalf SecondHalf, quarterHalf=r}}
--         Minuswards ->
--           case q of
--             SegmentQuarter FirstHalf FirstHalf ->
--               File{color=next c,
--                     colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter SecondHalf SecondHalf,quarterHalf=r}}
--             SegmentQuarter SecondHalf SecondHalf ->
--               File{color=prev c,
--                    colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter SecondHalf FirstHalf, quarterHalf=r}}
--             SegmentQuarter SecondHalf FirstHalf ->
--               File{color=prev c,
--                    colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter FirstHalf SecondHalf, quarterHalf=r}}
--             SegmentQuarter FirstHalf SecondHalf ->
--               File{color=prev c,
--                    colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter FirstHalf FirstHalf, quarterHalf=r}}
--               }
filewiseInc :: FilewiseDirection -> File -> File
filewiseInc Pluswards = plus
filewiseInc Minuswards = minus
-- add UnitDiagonalMove {direction=(DiagonalDirection Inwards p), count=Once} (Pos rank file) =
--   Just Pos {rank=inw rank, file=filewiseInc p file}
-- add UnitDiagonalMove {direction=(DiagonalDirection Outwards p), count=Once} (Pos rank file) =
--   do { o <- out rank; return Pos {rank=o, file=filewiseInc p file}}
-- add LinearMove m p = foldl add p $ unitsInvolRank m
