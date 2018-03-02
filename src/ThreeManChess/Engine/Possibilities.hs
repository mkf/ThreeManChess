{-# Language DuplicateRecordFields,GADTs,RankNTypes,DeriveDataTypeable,ScopedTypeVariables #-}
module ThreeManChess.Engine.Possibilities where

import Data.Data
import Data.Maybe
import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.Figure
import ThreeManChess.Engine.FigType

class --(Eq a , Read a, Show a) =>
  Reversable a where
  rever :: a -> a
class (Reversable a, Eq a-- , Ord a
      ) => LinearDirection a where
  tailRankInvol :: LinearVec a -> Maybe (Either (Rank -> (LinearVec a)) (LinearVec a))
  addOne :: a -> Pos -> Maybe Pos
data Orientation = Rankwise | Filewise deriving (Eq, Read, Show)
perpendicularTo :: Orientation -> Orientation
perpendicularTo Rankwise = Filewise
perpendicularTo Filewise = Rankwise
class (LinearDirection a) => StraightDirection a where
  orientation :: a -> Orientation
data RankwiseDirection = Inwards | Outwards deriving (Data, Typeable)-- deriving StraightDirection
instance Reversable RankwiseDirection where
  rever Inwards = Outwards
  rever Outwards = Inwards
instance StraightDirection RankwiseDirection where
  orientation _ = Rankwise
instance LinearDirection RankwiseDirection where
  tailRankInvol (LinearVec _ Once) = Nothing
  tailRankInvol (LinearVec Inwards (OnceMore c)) = Just $ Left (\x -> LinearVec (case x of MostInner -> Outwards
                                                                                           _ -> Inwards) c)
  tailRankInvol (LinearVec Outwards (OnceMore c)) = Just $ Right (LinearVec Outwards c)
  addOne Inwards (Pos MostInner file) = Just (Pos MostInner $ opposite file)
  addOne Inwards (Pos rank file) = Just (Pos (inw rank) file)
  addOne Outwards (Pos rank file) =
    do { o <- out rank;
         return (Pos o file) }
instance Eq RankwiseDirection where
  Inwards == Inwards = True
  Outwards == Outwards = True
  Inwards == Outwards = False
  Outwards == Inwards = False
data FilewiseDirection = Pluswards | Minuswards deriving (Data, Typeable)-- deriving StraightDirection
instance Reversable FilewiseDirection where
  rever Pluswards = Minuswards
  rever Minuswards = Pluswards
instance StraightDirection FilewiseDirection where
  orientation _ = Filewise
filewiseInc :: FilewiseDirection -> File -> File
filewiseInc Pluswards = plus
filewiseInc Minuswards = minus
instance LinearDirection FilewiseDirection where
  tailRankInvol (LinearVec _ Once) = Nothing
  tailRankInvol (LinearVec d (OnceMore c)) = Just $ Right (LinearVec d c)
  addOne w (Pos rank file) = Just (Pos rank $ filewiseInc w file)
instance Eq FilewiseDirection where
  Pluswards == Pluswards = True
  Minuswards == Minuswards = True
  Pluswards == Minuswards = False
  Minuswards == Pluswards = False
data DiagonalDirection = DiagonalDirection RankwiseDirection FilewiseDirection -- deriving LinearDirection
rankwise :: DiagonalDirection -> RankwiseDirection
rankwise (DiagonalDirection x _) = x
filewise :: DiagonalDirection -> FilewiseDirection
filewise (DiagonalDirection _ x) = x
instance LinearDirection DiagonalDirection where
  tailRankInvol (LinearVec _ Once) = Nothing
  tailRankInvol (LinearVec (DiagonalDirection Inwards f) (OnceMore c)) =
    Just $ Left (\x -> LinearVec (case x of MostInner -> (DiagonalDirection Outwards (rever f))
                                            _ -> (DiagonalDirection Inwards f)) c)
  tailRankInvol (LinearVec d (OnceMore c)) = Just $ Right (LinearVec d c)
  addOne (DiagonalDirection Inwards p) (Pos MostInner (File c (SegmentEight q r))) =
    Just
    (Pos MostInner
      (case p of
          Pluswards ->
            case q of
              SegmentQuarter SecondHalf SecondHalf ->
                File{segmColor=prev c,
                     colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter FirstHalf FirstHalf,quarterHalf=r}}
              SegmentQuarter SecondHalf FirstHalf ->
                File{segmColor=next c,
                     colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter SecondHalf SecondHalf, quarterHalf=r}}
              SegmentQuarter FirstHalf SecondHalf ->
                File{segmColor=next c,
                     colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter SecondHalf FirstHalf, quarterHalf=r}}
              SegmentQuarter FirstHalf FirstHalf ->
                File{segmColor=next c,
                     colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter FirstHalf SecondHalf, quarterHalf=r}}
          Minuswards ->
            case q of
              SegmentQuarter FirstHalf FirstHalf ->
                File{segmColor=next c,
                     colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter SecondHalf SecondHalf,quarterHalf=r}}
              SegmentQuarter SecondHalf SecondHalf ->
                File{segmColor=prev c,
                     colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter SecondHalf FirstHalf, quarterHalf=r}}
              SegmentQuarter SecondHalf FirstHalf ->
                File{segmColor=prev c,
                     colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter FirstHalf SecondHalf, quarterHalf=r}}
              SegmentQuarter FirstHalf SecondHalf ->
                File{segmColor=prev c,
                     colorSegmFile=SegmentEight{segmentQuarter=SegmentQuarter FirstHalf FirstHalf, quarterHalf=r}}
      ))
  addOne (DiagonalDirection Inwards p) (Pos rank file) = Just (Pos (inw rank) (filewiseInc p file))
  addOne (DiagonalDirection Outwards p) (Pos rank file) =
    do { o <- out rank; return (Pos o (filewiseInc p file))}
instance Eq DiagonalDirection where
  (DiagonalDirection a b) == (DiagonalDirection c d) = (a == c) && (b == d)
instance Reversable DiagonalDirection where
  rever (DiagonalDirection a b) = DiagonalDirection (rever a) (rever b)
data Count = Once | OnceMore Count deriving (Eq, Read, Show)
addCount :: Count -> Count -> Count
addCount Once Once = OnceMore Once
addCount a Once = OnceMore a
addCount Once a = OnceMore a
addCount (OnceMore a) (OnceMore b) = OnceMore $ OnceMore $ addCount a b
instance Ord Count where
  Once `compare` Once = EQ
  OnceMore a `compare` OnceMore b = a `compare` b
  Once `compare` OnceMore _ = LT
  OnceMore _ `compare` Once = GT
class (Eq a-- , Read a, Show a
      ) => Vec a where
  reverMaybe :: a -> Maybe a
  add :: Pos -> a -> Maybe Pos
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
  add (Pos MostOuter (File c (SegmentEight (SegmentQuarter SecondHalf FirstHalf) FirstHalf))) QueensideCastling =
    Just (Pos MostOuter File{segmColor=c,
                             colorSegmFile=SegmentEight{
                                segmentQuarter=SegmentQuarter {
                                    half=FirstHalf, halfQuarter=SecondHalf}, quarterHalf=FirstHalf}})
  add _ QueensideCastling = undefined
  add (Pos MostOuter (File c (SegmentEight (SegmentQuarter SecondHalf FirstHalf) FirstHalf))) KingsideCastling =
    Just (Pos MostOuter File{segmColor=c,
                             colorSegmFile=SegmentEight{
                                segmentQuarter=SegmentQuarter {
                                    half=SecondHalf, halfQuarter=SecondHalf}, quarterHalf=FirstHalf}})
  add _ KingsideCastling = undefined
data PawnJumpByTwo = PawnJumpByTwo --deriving (Vec)
instance Eq PawnJumpByTwo where
  PawnJumpByTwo == PawnJumpByTwo = True
instance Vec PawnJumpByTwo where
  reverMaybe PawnJumpByTwo = Nothing
  add (Pos SecondOuter f) PawnJumpByTwo = Just (Pos MiddleInner f)
  add _ PawnJumpByTwo = Nothing
-- data (LinearDirection a) => LinearVec a = LinearVec a Count --deriving (Ord)
data LinearVec a where
  LinearVec :: LinearDirection a => a -> Count -> LinearVec a
direction :: (LinearDirection a) => LinearVec a -> a
direction (LinearVec d _) = d
count :: (LinearDirection a) => LinearVec a -> Count
count (LinearVec _ c) = c
-- instance (LinearDirection a) => Ord (LinearVec a)
instance (LinearDirection a) => ReversableVec (LinearVec a)
instance (LinearDirection a) => Reversable (LinearVec a) where
  rever (LinearVec d n) = (LinearVec (rever d) n)
instance (LinearDirection t) => Eq (LinearVec t) where
  (LinearVec a c) == (LinearVec b d) = (a == b) && (c == d)
instance (LinearDirection a) => Vec (LinearVec a) where
  reverMaybe x = Just $ rever x
  add p (LinearVec d Once) = addOne d p
  add p m = foldl _addMaybe (Just p) (unitsInvolRank m (rank p))
_addMaybe :: (LinearDirection a) => Maybe Pos -> (LinearVec a) -> Maybe Pos
_addMaybe p m = do { jp <- p;
                     o <- add jp m;
                     return o}
data KnightVec = KnightVec RankwiseDirection FilewiseDirection Orientation -- Orientation :: twice
-- instance Ord KnightVec
instance Eq KnightVec where
  (KnightVec r f o) == (KnightVec rr ff oo) = (r==rr)&&(f==ff)&&(o==oo)
instance ReversableVec KnightVec
instance Reversable KnightVec where
  rever (KnightVec r f orient) = (KnightVec (rever r) (rever f) orient)
instance Vec KnightVec where
  reverMaybe x = Just $ rever x
-- add KnightMove {rankwise=r, filewise=f, twice=t} x =
--   do { fl <- add UnitStraightMove {direction=case t of Rankwise -> r; Filewise -> f} x;
--        return add UnitStraightMove {direction=case t of Filewise -> r; Rankwise -> f} fl }
  add x (KnightVec r f t) =
    do { x <- addOne r x;
         x <- addOne f x;
         x <- (case t of Rankwise -> addOne r; Filewise -> addOne f) x;
         return x;}

-- add LinearMove m p = foldl add p $ unitsInvolRank m
-- class StraightVec
-- class (StraightDirection a) => StraightVec a
-- instance (StraightDirection a) => StraightVec (LinearVec a)
-- type (StraightDirection a) => StraightVec a = LinearVec a

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
headLinear :: (LinearDirection a) => LinearVec a -> LinearVec a --UnitLinearVec
headLinear (LinearVec d _) = LinearVec d Once
tailFilewise :: LinearVec FilewiseDirection -> Maybe (LinearVec FilewiseDirection)
tailFilewise (LinearVec _ Once) = Nothing
tailFilewise (LinearVec d (OnceMore c)) = Just (LinearVec d c)
-- units :: LinearMove -> Either (Rank -> [UnitRankInvolMove]) [UnitLinearMove]
cons :: a -> ([a] -> [a])
cons a as = a:as
units :: (LinearDirection a) => LinearVec a -> Either (Rank -> [LinearVec a]) [LinearVec a]
units x = let
  {t = tailRankInvol x;
   h = headLinear x} in
    case t of
      Just (Right justTail) -> let ur = units justTail in case ur of
        Right justRestOfUnits -> Right $ h:justRestOfUnits
        _ -> undefined
      Just (Left tailByRank) ->
        Left (\rankArg ->
                let ul = units $ tailByRank rankArg in case ul of
                  Right right -> h:right
                  Left restByRank -> cons h $ restByRank rankArg)
      Nothing -> Right []
-- unitsInvolRank :: LinearMove -> Rank -> [UnitLinearMove]
unitsInvolRank :: (LinearDirection a) => LinearVec a -> Rank -> [LinearVec a]
unitsInvolRank x = either id const $ units x
-- fromToVecsByFig :: FigType -> Pos -> Pos -> [Vec a]
-- fromToVecsByFig Queen a b = (fromToVecs Rook a b) ++ (fromToVecs Bishop a b)
-- fromToVecsByFig Rook
-- fromToVecsByFig Bishop
rankOnceWards :: RankwiseDirection -> Rank -> Maybe Rank
rankOnceWards Inwards = Just . inw
rankOnceWards Outwards = out
fromToRanks :: Rank -> Rank -> Maybe (LinearVec RankwiseDirection)
fromToRanks a b = case (compare a b) of
  EQ -> Nothing
  co -> Just $ let { d = case co of LT -> Inwards; GT -> Outwards } in
                 case do { row <- rankOnceWards d a;
                           ftr <- fromToRanks row b;
                           return ftr } of
                   Nothing -> LinearVec d Once
                   Just (LinearVec d om) -> LinearVec d (OnceMore om)
fromToRankwise :: Pos -> Pos -> [LinearVec RankwiseDirection]
fromToRankwise (Pos a b) (Pos c d)
  | b == d = maybeToList (fromToRanks a c)
  | opposite b == d = maybeToList $ do { t <- (case a of
                                                MostInner -> Just $ LinearVec Inwards Once
                                                a -> fromToRanks a MostInner);
                                         o <- fromToRanks MostInner c;
                                         return $ LinearVec Inwards (addCount (count t) (count o)) }
  | otherwise = []
fromToFilesWards :: FilewiseDirection -> File -> File -> Maybe Count
fromToFilesWards w a b | a==b = Nothing
  | otherwise = let wf = filewiseInc w in
  let c = (wf a) in
    if c==b then Just Once
    else do { n <- fromToFilesWards w c b;
              return $ OnceMore n }
fromToFiles :: File -> File -> Maybe (LinearVec FilewiseDirection, LinearVec FilewiseDirection)
fromToFiles a b = do { p <- fromToFilesWards Pluswards a b;
                       m <- fromToFilesWards Minuswards a b;
                       return $ let { pv = LinearVec Pluswards p;
                                      mv = LinearVec Minuswards m } in
                                  if m<p then (mv,pv) else (pv,mv) }
fromToFilewise :: Pos -> Pos -> [LinearVec FilewiseDirection]
fromToFilewise (Pos a b) (Pos c d)
  | a == c = case do { f <- fromToFiles b d;
                       return [(fst f), (snd f)] } of
               Just f -> f
               Nothing -> []
  | otherwise = []
type StraightVecsOfKinds = ([LinearVec RankwiseDirection], [LinearVec FilewiseDirection])
type LinearVecsOfKinds = (StraightVecsOfKinds, [LinearVec DiagonalDirection])
fromToStraight :: Pos -> Pos -> StraightVecsOfKinds
fromToStraight a b = ( (fromToRankwise a b) , (fromToFilewise a b) )
