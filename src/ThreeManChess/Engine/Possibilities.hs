{-# Language DuplicateRecordFields,GADTs,RankNTypes,DeriveDataTypeable,ScopedTypeVariables,ExistentialQuantification #-}
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
  tailRankInvol :: LinearVec a -> Maybe (Either (Rank -> LinearVec a) (LinearVec a))
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
  addOne Inwards (MostInner, file) = Just (MostInner, opposite file)
  addOne Inwards (rank, file) = Just (inw rank, file)
  addOne Outwards (rank, file) =
    do { o <- out rank;
         return (o, file) }
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
  addOne w (rank, file) = Just (rank, filewiseInc w file)
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
    Just $ Left (\x -> LinearVec (case x of MostInner -> DiagonalDirection Outwards (rever f)
                                            _ -> DiagonalDirection Inwards f) c)
  tailRankInvol (LinearVec d (OnceMore c)) = Just $ Right (LinearVec d c)
  addOne (DiagonalDirection Inwards p) (MostInner, File c (SegmentEight q r)) =
    Just
    (MostInner,
     case p of
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
    )
  addOne (DiagonalDirection Inwards p) (rank, file) = Just (inw rank, filewiseInc p file)
  addOne (DiagonalDirection Outwards p) (rank, file) =
    do { o <- out rank; return (o, filewiseInc p file)}
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
countFromPositiveInteger :: Integer -> Maybe Count
countFromPositiveInteger 1 = Just Once
countFromPositiveInteger a | a>1 = OnceMore <$> countFromPositiveInteger (a-1)
                           | otherwise = Nothing
data PlusMinus a = Plus a | Minus a deriving (Eq, Read, Show)
absPlusMinus :: PlusMinus a -> a
absPlusMinus (Plus a) = a
absPlusMinus (Minus a) = a
substractCount :: Count -> Count -> Maybe (PlusMinus Count)
substractCount Once Once = Nothing
substractCount (OnceMore a) Once = Just $ Plus a
substractCount Once (OnceMore a) = Just $ Minus a
substractCount (OnceMore a) (OnceMore b) = substractCount a b
absSubstractCount :: Count -> Count -> Maybe Count
absSubstractCount = curry $ (fmap absPlusMinus).(uncurry substractCount)
nonNegativeSubstractCount :: Count -> Count -> Maybe Count
nonNegativeSubstractCount Once Once = Nothing
nonNegativeSubstractCount (OnceMore a) Once = Just a
nonNegativeSubstractCount (OnceMore a) (OnceMore b) = nonNegativeSubstractCount a b
nonNegativeSubstractCount _ _ = undefined
positiveSubstractCount :: Count -> Count -> Count
positiveSubstractCount (OnceMore a) Once = a
positiveSubstractCount (OnceMore a) (OnceMore b) = positiveSubstractCount a b
positiveSubstractCount _ _ = undefined
instance Ord Count where
  Once `compare` Once = EQ
  OnceMore a `compare` OnceMore b = a `compare` b
  Once `compare` OnceMore _ = LT
  OnceMore _ `compare` Once = GT
class (Eq a-- , Read a, Show a
      ) => Vec a where
  reverMaybe :: a -> Maybe a
  add :: Pos -> a -> Maybe Pos
-- data VecC = forall a . Vec a => MkVecC a
data VecC where
  MkVecC :: Vec a => a -> VecC
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
  add (MostOuter, File c (SegmentEight (SegmentQuarter SecondHalf FirstHalf) FirstHalf)) QueensideCastling =
    Just (MostOuter, File{segmColor=c,
                             colorSegmFile=SegmentEight{
                                segmentQuarter=SegmentQuarter {
                                    half=FirstHalf, halfQuarter=SecondHalf}, quarterHalf=FirstHalf}})
  add _ QueensideCastling = undefined
  add (MostOuter, File c (SegmentEight (SegmentQuarter SecondHalf FirstHalf) FirstHalf)) KingsideCastling =
    Just (MostOuter, File{segmColor=c,
                             colorSegmFile=SegmentEight{
                                segmentQuarter=SegmentQuarter {
                                    half=SecondHalf, halfQuarter=SecondHalf}, quarterHalf=FirstHalf}})
  add _ KingsideCastling = undefined
data PawnJumpByTwo = PawnJumpByTwo --deriving (Vec)
instance Eq PawnJumpByTwo where
  PawnJumpByTwo == PawnJumpByTwo = True
instance Vec PawnJumpByTwo where
  reverMaybe PawnJumpByTwo = Nothing
  add (SecondOuter, f) PawnJumpByTwo = Just (MiddleInner, f)
  add _ PawnJumpByTwo = Nothing
-- data (LinearDirection a) => LinearVec a = LinearVec a Count --deriving (Ord)
data LinearVec a where
  LinearVec :: LinearDirection a => a -> Count -> LinearVec a
data LinearVecC = forall a . LinearDirection a => MkLinearVecC (LinearVec a)
-- data LinearVecC where
--   MkLinearVecC :: LinearDirection a => a -> LinearVecC (LinearVec a)
data StraightVecC = forall a . StraightDirection a => MkStraightVecC (LinearVec a)
-- data StraightVecC where
--   MkStraightVecC :: StraightDirection a => a -> StraightVecC (LinearVec a)
direction :: (LinearDirection a) => LinearVec a -> a
direction (LinearVec d _) = d
count :: (LinearDirection a) => LinearVec a -> Count
count (LinearVec _ c) = c
-- instance (LinearDirection a) => Ord (LinearVec a)
instance (LinearDirection a) => ReversableVec (LinearVec a)
instance (LinearDirection a) => Reversable (LinearVec a) where
  rever (LinearVec d n) = LinearVec (rever d) n
instance (LinearDirection t) => Eq (LinearVec t) where
  (LinearVec a c) == (LinearVec b d) = (a == b) && (c == d)
instance (LinearDirection a) => Vec (LinearVec a) where
  reverMaybe x = Just $ rever x
  add p (LinearVec d Once) = addOne d p
  add p m = foldl _addMaybe (Just p) (unitsInvolRank m (rank p))
_addMaybe :: (LinearDirection a) => Maybe Pos -> LinearVec a -> Maybe Pos
_addMaybe p m = do { jp <- p;
                     add jp m;}
data KnightVec = KnightVec RankwiseDirection FilewiseDirection Orientation -- Orientation :: twice
-- instance Ord KnightVec
instance Eq KnightVec where
  (KnightVec r f o) == (KnightVec rr ff oo) = (r==rr)&&(f==ff)&&(o==oo)
instance ReversableVec KnightVec
instance Reversable KnightVec where
  rever (KnightVec r f orient) = KnightVec (rever r) (rever f) orient
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
fromToRanks :: (Rank, Rank) -> Maybe (LinearVec RankwiseDirection)
fromToRanks (a,b) = case compare a b of
  EQ -> Nothing
  co -> Just $ let { d = case co of LT -> Inwards; GT -> Outwards } in
                 case do { row <- rankOnceWards d a;
                           curry fromToRanks row b;} of
                   Nothing -> LinearVec d Once
                   Just (LinearVec d om) -> LinearVec d (OnceMore om)
fromToRankwise :: Pos -> Pos -> [LinearVec RankwiseDirection]
fromToRankwise (a, b) (c, d)
  | b == d = maybeToList (fromToRanks (a,c))
  | opposite b == d = maybeToList $ do { t <- (case a of
                                                MostInner -> Just $ LinearVec Inwards Once
                                                a -> fromToRanks (a,MostInner));
                                         o <- fromToRanks (MostInner,c);
                                         return $ LinearVec Inwards (addCount (count t) (count o)) }
  | otherwise = []
fromToFilesWards :: FilewiseDirection -> File -> File -> Maybe Count
fromToFilesWards w a b | a==b = Nothing
  | otherwise = let wf = filewiseInc w in
  let c = wf a in
    if c==b then Just Once
    else OnceMore <$> fromToFilesWards w c b
fromToFiles :: (File, File) -> Maybe (LinearVec FilewiseDirection, LinearVec FilewiseDirection)
fromToFiles (a,b) = do { p <- fromToFilesWards Pluswards a b;
                         m <- fromToFilesWards Minuswards a b;
                         return $ let { pv = LinearVec Pluswards p;
                                        mv = LinearVec Minuswards m } in
                                    if m<p then (mv,pv) else (pv,mv) }
fromToFilewise :: Pos -> Pos -> [LinearVec FilewiseDirection]
fromToFilewise (a, b) (c, d)
  | a == c = fromMaybe [] $ do { f <- fromToFiles (b,d);
                                 return [fst f, snd f] }
  | otherwise = []
-- type StraightVecsOfKinds = ([LinearVec RankwiseDirection], [LinearVec FilewiseDirection])
-- type LinearVecsOfKinds = (StraightVecsOfKinds, [LinearVec DiagonalDirection])
-- fromToStraight :: Pos -> Pos -> StraightVecsOfKinds
fromToStraight :: Pos -> Pos -> [StraightVecC]
fromToStraight a b = (map MkStraightVecC (fromToRankwise a b)) ++ (map MkStraightVecC (fromToFilewise a b))
-- fromToDiagWards :: DiagonalDirection -> Pos -> Pos -> Maybe Count
-- fromToDiagWards (DiagonalDirection Outwards _) (MostOuter, _) _ = Nothing
-- fromToDiagWards (DiagonalDirection Inwards w) (MostInner, a) (MostInner, b)
--   | (let f = filewiseInc w in
--        a == ((f.f.f.f.f . f.f.f.f.f) b)) = Just Once
--   | otherwise = Nothing
bothMaybe :: (Maybe a, Maybe a) -> Maybe (a, a)
bothMaybe (m, n) = do { m <- m; n <- n; return (m,n); }
mapHomoTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapHomoTuple2 f (a,b) = (f a, f b)
sumCountPair :: (Count, Count) -> Count
sumCountPair = uncurry addCount
addMaybeCount :: Maybe Count -> Maybe Count -> Maybe Count
addMaybeCount Nothing a = a
addMaybeCount a Nothing = a
addMaybeCount (Just a) (Just b) = Just $ addCount a b
sumMaybeCountPair :: (Maybe Count, Maybe Count) -> Maybe Count
sumMaybeCountPair = uncurry addMaybeCount
filewiseToShortDiagonal :: LinearVec FilewiseDirection -> RankwiseDirection -> LinearVec DiagonalDirection
filewiseToShortDiagonal (LinearVec f c) r = LinearVec (DiagonalDirection r f) c
maybeIf :: (a -> Bool) -> Maybe a -> Maybe a
maybeIf f (Just v) = if f v then Just v else Nothing
maybeIf _ Nothing = Nothing
fromToFilesShort :: (File, File) -> Maybe (LinearVec FilewiseDirection)
fromToFilesShort = fmap fst . fromToFiles
fromToShortDiagonal :: Pos -> Pos -> Maybe (LinearVec DiagonalDirection)
fromToShortDiagonal a b = do
  fileDiff <- fromToFilesShort (file a, file b)
  fmap (filewiseToShortDiagonal fileDiff . direction)
    (maybeIf
      ((count fileDiff ==).count)
      (fromToRanks (rank a, rank b)))
filewiseToLongDiagonal :: LinearVec FilewiseDirection -> LinearVec DiagonalDirection
filewiseToLongDiagonal (LinearVec f fileCount) =
  LinearVec (DiagonalDirection Inwards (rever f)) (positiveSubstractCount (fromJust $ countFromPositiveInteger (5+5+1)) fileCount)
fileDistance :: (File,File) -> Maybe Count
fileDistance = fmap count . fromToFilesShort
fileCoorDistance :: (Pos,Pos) -> Maybe Count
fileCoorDistance = fileDistance . mapHomoTuple2 file
fromToCoorFilesShort :: (Pos,Pos) -> Maybe (LinearVec FilewiseDirection)
fromToCoorFilesShort = fromToFilesShort . mapHomoTuple2 file
fromToCoorRanks :: (Pos,Pos) -> Maybe (LinearVec RankwiseDirection)
fromToCoorRanks = fromToRanks. mapHomoTuple2 rank
rankDistance :: (Rank, Rank) -> Maybe Count
rankDistance = fmap count . fromToRanks
rankDistanceFromMostOuter :: Rank -> Maybe Count
rankDistanceFromMostOuter = fmap count . curry fromToRanks MostOuter
sumOfRankDistancesFromMostOuter :: (Rank, Rank) -> Maybe Count
sumOfRankDistancesFromMostOuter =
  sumMaybeCountPair . mapHomoTuple2 rankDistanceFromMostOuter
sumOfRankCoorDistancesFromMostOuter :: (Pos,Pos) -> Maybe Count
sumOfRankCoorDistancesFromMostOuter =
  sumMaybeCountPair . mapHomoTuple2 (rankDistanceFromMostOuter.rank)
isFilewiseDistanceSameAsSumOfRanksDistancesFromMostOuter :: (Pos,Pos) -> Bool
isFilewiseDistanceSameAsSumOfRanksDistancesFromMostOuter x =
  maybe False (uncurry (==)) $ bothMaybe (fileCoorDistance x, sumOfRankCoorDistancesFromMostOuter x)
fromToLongDiagonal :: Pos -> Pos -> Maybe (LinearVec DiagonalDirection)
-- fromToLongDiagonal a b = do
--   fileDiff <- fromToFilesShort (file a) (file b)
--   rankSum <- sumCountPair (mapHomoTuple2 ((fmap count).(fromToRank MostOuter).rank) (a,b))
--   if (rankSum == (count fileDiff)) then (filewiseToLongDiagonal fileDiff) else Nothing
fromToLongDiagonal a b
  | isFilewiseDistanceSameAsSumOfRanksDistancesFromMostOuter (a,b)
    = fmap filewiseToLongDiagonal (fromToCoorFilesShort (a,b))
  | otherwise = Nothing
fromToDiagonals :: Pos -> Pos -> (Maybe (LinearVec DiagonalDirection), Maybe (LinearVec DiagonalDirection))
fromToDiagonals a b = (fromToShortDiagonal a b, fromToLongDiagonal a b)
fromToDiagonal :: Pos -> Pos -> [LinearVec DiagonalDirection]
fromToDiagonal a b = let s = fromToDiagonals a b in uncurry (++) (mapHomoTuple2 maybeToList s)
