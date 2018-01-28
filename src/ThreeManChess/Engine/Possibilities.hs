module ThreeManChess.Engine.Possibilities where

import Data.Data
import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.Figure

data RankwiseDirection = Inwards | Outwards deriving (Eq, Ord, Read, Show)
data FilewiseDirection = Pluswards | Minuswards deriving (Eq, Ord, Read, Show)
data StraightDirection = RankwiseDirection | FilewiseDirection deriving (Eq, Ord, Read, Show)
data DiagonalDirection = DiagonalDirection {rankwise :: RankwiseDirection, filewise :: FilewiseDirection}
                       deriving (Eq, Ord, Read, Show)
data LinearDirection = StraightDirection | DiagonalDirection deriving (Eq, Ord, Read, Show)
reverse :: LinearDirection -> LinearDirection
reverse Inwards = Outwards
reverse Outwards = Inwards
reverse Pluswards = Minuswards
reverse Minuswards = Pluswards
reverse (DiagonalDirection a b) = DiagonalDirection reverse a $ reverse b
data Orientation = Rankwise | Filewise deriving (Eq, Ord, Read, Show)
data Count = Once | OnceMore Count deriving (Eq, Ord, Read, Show)
data KnightMove = KnightMove {rankwise :: RankwiseDirection, filewise :: FilewiseDirection, twice :: Orientation}
                deriving (Eq, Ord, Read, Show)
data Castling = QueensideCastling | KingsideCastling
data LinearMove = LinearMove {direction :: LinearDirection, count :: Count} deriving (Eq, Ord, Read, Show)
data Move = LinearMove | KnightMove | Castling
newtype StraightMove = LinearMove { direction :: StraightDirection }
                  deriving (Eq, Ord, Read, Show, LinearMove)
newtype DiagonalMove = LinearMove { direction :: DiagonalDirection }
                  deriving (Eq, Ord, Read, Show, LinearMove)
newtype RankwiseMove = StraightMove { direction :: RankwiseDirection }
                         deriving (Eq, Ord, Read, Show, StraightMove )
newtype FilewiseMove = StraightMove { direction :: FilewiseDirection }
                         deriving (Eq, Ord, Read, Show, StraightMove )
data RankInvolMove = RankwiseMove | DiagonalMove deriving (Eq, Ord, Read, Show, LinearMove)
newtype UnitLinearMove = LinearMove { count :: Once } deriving (Eq, Ord, Read, Show, LinearMove)
newtype UnitStraightMove = UnitLinearMove { direction :: StraightDirection }
                         deriving (Eq, Ord, Read, Show, UnitLinearMove, StraightMove)
newtype UnitDiagonalMove = UnitLinearMove { direction :: DiagonalDirection }
                         deriving (Eq, Ord, Read, Show, UnitLinearMove, DiagonalMove)
newtype UnitRankwiseMove = UnitStraightMove { direction :: RankwiseDirection }
                         deriving (Eq, Ord, Read, Show, UnitStraightMove, RankwiseMove )
newtype UnitFilewiseMove = UnitStraightMove { direction :: FilewiseDirection }
                         deriving (Eq, Ord, Read, Show, UnitStraightMove, FilewiseMove )
data UnitRankInvolMove = UnitRankwiseMove | UnitDiagonalMove deriving (Eq, Ord, Read, Show, UnitLinearMove, RankInvolMove)
headLinear :: LinearMove -> UnitLinearMove
headLinear LinearMove {direction=d} = UnitLinearMove {direction=d}
tailFilewise :: FilewiseMove -> Maybe UnitStraightMove
tailFilewise FilewiseMove {count=Once} = Nothing
tailFilewise FilewiseMove {direction=d, count=OnceMore c} = FilewiseMove {direction=d, count=c}
tailRankInvol :: LinearMove -> Maybe Either (Rank -> RankInvolMove) LinearMove
tailRankInvol RankInvolMove {count=Once} = Nothing
tailRankInvol RankwiseMove {direction=Inwards, count=OnceMore c} =
  Just Left (\x -> RankwiseMove {direction =
                               case x of MostInner -> Outwards
                                         _ -> Inwards,
                            count=c})
tailRankInvol DiagonalMove {direction=DiagonalDirection {rankwise=Inwards, filewise=f}, count=OnceMore c} =
  Just Left (\x -> DiagonalMove {direction=
                               case x of MostInner -> DiagonalDirection{
                                           rankwise=Outwards, filewise=reverse f}
                                         _ -> DiagonalDirection{rankwise=Inwards, filewise=f},
                            count=c})
tailRankInvol LinearMove {direction=d, count=OnceMore c} = Just Right LinearMove {direction=d, count=c}
units :: LinearMove -> Either (Rank -> [UnitRankInvolMove]) [UnitLinearMove]
units x = let
  {t = tailRankInvol x;
   h = headLinear x} in h :
    case t of
      Just e -> either (\left rank -> units $ left rank) units  e
      Nothing -> []
unitsInvolRank :: LinearMove -> Rank -> [UnitLinearMove]
unitsInvolRank x = either id const $ units x

add :: Move -> Pos -> Maybe Pos
add KnightMove {rankwise=r, filewise=f, twice=t} x =
  do { fl <- add UnitStraightMove {direction=case t of Rankwise -> r; Filewise -> f} x;
       return add UnitStraightMove {direction=case t of Filewise -> r; Rankwise -> f} fl }
add QueensideCastling (Pos MostOuter (File c (SegmentEight (SegmentQuarter SecondHalf FirstHalf) FirstHalf))) =
  Just Pos {rank=MostOuter, file=File{color=c,
                                      colorSegmFile=SegmentEight{
                                         segmentQuarter=SegmentQuarter {
                                             half=FirstHalf, halfQuarter=SecondHalf}, quarterHalf=FirstHalf}}}
add QueensideCastling _ = undefined
add KingsideCastling (Pos MostOuter (File c (SegmentEight (SegmentQuarter SecondHalf FirstHalf) FirstHalf))) =
  Just Pos {rank=MostOuter, file=File{color=c,
                                      colorSegmFile=SegmentEight{
                                         segmentQuarter=SegmentQuarter {
                                             half=SecondHalf, halfQuarter=SecondHalf}, quarterHalf=FirstHalf}}}
add KingsideCastling _ = undefined
add UnitRankwiseMove {direction=Inwards, count=Once} (Pos MostInner file) = Just Pos {rank=MostInner,file=opposite file}
add UnitRankwiseMove {direction=Inwards, count=Once} (Pos rank file) = Just Pos {rank=inw rank,file=file}
add UnitRankwiseMove {direction=Outwards, count=Once} (Pos rank file) =
  do { o <- out rank;
       return Pos {rank=o,file=file} }
add UnitFilewiseMove {direction=Pluswards, count=Once} (Pos rank file) = Just Pos {rank=rank, file=plus file}
add UnitFilewiseMove {direction=Minuswards, count=Once} (Pos rank file) = Just Pos {rank=rank, file=minus file}
add UnitDiagonalMove {direction=(DiagonalDirection Inwards p), count=Once}
                     (Pos MostInner (File c (SegmentEight q r))) =
  Just Pos{
  rank=MostInner,
  file =
      case p of
        Pluswards ->
          case q of
            SegmentQuarter SecondHalf SecondHalf ->
              File{color=prev c,
                    colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter FirstHalf FirstHalf,quarterHalf=r}}
            SegmentQuarter SecondHalf FirstHalf ->
              File{color=next c,
                   colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter SecondHalf SecondHalf, quarterHalf=r}}
            SegmentQuarter FirstHalf SecondHalf ->
              File{color=next c,
                   colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter SecondHalf FirstHalf, quarterHalf=r}}
            SegmentQuarter FirstHalf FirstHalf ->
              File{color=next c,
                   colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter FirstHalf SecondHalf, quarterHalf=r}}
        Minuswards ->
          case q of
            SegmentQuarter FirstHalf FirstHalf ->
              File{color=next c,
                    colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter SecondHalf SecondHalf,quarterHalf=r}}
            SegmentQuarter SecondHalf SecondHalf ->
              File{color=prev c,
                   colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter SecondHalf FirstHalf, quarterHalf=r}}
            SegmentQuarter SecondHalf FirstHalf ->
              File{color=prev c,
                   colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter FirstHalf SecondHalf, quarterHalf=r}}
            SegmentQuarter FirstHalf SecondHalf ->
              File{color=prev c,
                   colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter FirstHalf FirstHalf, quarterHalf=r}}
              }
filewiseInc :: FilewiseDirection -> File -> File
filewiseInc Pluswards = plus
filewiseInc Minuswards = minus
add UnitDiagonalMove {direction=(DiagonalDirection Inwards p), count=Once} (Pos rank file) =
  Just Pos {rank=inw rank, file=filewiseInc p file}
add UnitDiagonalMove {direction=(DiagonalDirection Outwards p), count=Once} (Pos rank file) =
  do { o <- out rank; return Pos {rank=o, file=filewiseInc p file}}
add LinearMove m p = foldl add p $ unitsInvolRank m
