{-# Language DuplicateRecordFields,GADTs,RankNTypes,ScopedTypeVariables,ExistentialQuantification,
             LiberalTypeSynonyms,NamedFieldPuns,RecordWildCards,TypeFamilies,PatternSynonyms,FunctionalDependencies,
             FlexibleInstances,InstanceSigs,TypeFamilyDependencies,DataKinds,PolyKinds,ConstraintKinds,KindSignatures,
             TypeApplications,ImplicitParams,ImpredicativeTypes,PartialTypeSignatures,NamedWildCards #-}
module ThreeManChess.Engine.Possibilities where

import Control.Exception
-- import Data.Data
import Data.Maybe
import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.Color
-- import ThreeManChess.Engine.Figure
-- import ThreeManChess.Engine.FigType
import ThreeManChess.Engine.Moats
import ThreeManChess.Engine.Directions


class (Reversable a, Eq a-- , Ord a
      ) => LinearDirection a where
  tailRankInvol :: LinearVec a -> Maybe (Either (Rank -> LinearVec a) (LinearVec a))
  addOne :: a -> Pos -> Maybe Pos
instance LinearDirection RankwiseDirection where
  tailRankInvol (LinearVec (_, 1)) = Nothing
  tailRankInvol (LinearVec (Inwards, c)) = Just $ Left (\x -> LinearVec (case x of 5 -> Outwards
                                                                                   _ -> Inwards, c-1))
  tailRankInvol (LinearVec (Outwards, c)) = Just $ Right $ LinearVec (Outwards, c-1)
  addOne Inwards (5, file) = Just (5, opposite file)
  addOne Inwards (rank, file) = Just (inw rank, file)
  addOne Outwards (rank, file) =
    do { o <- out rank;
         return (o, file) }
instance LinearDirection FilewiseDirection where
  tailRankInvol (LinearVec (_, 1)) = Nothing
  tailRankInvol (LinearVec (d, c)) = Just $ Right (LinearVec (d, c-1))
  addOne w (rank, file) = Just (rank, filewiseInc w file)
instance LinearDirection DiagonalDirection where
  tailRankInvol (LinearVec (_, 1)) = Nothing
  tailRankInvol (LinearVec (DiagonalDirection Inwards f, c)) =
    Just $ Left (\x -> LinearVec ((case x of 5 -> DiagonalDirection Outwards (rever f)
                                             _ -> DiagonalDirection Inwards f), c-1 ))
  tailRankInvol (LinearVec (d, c)) = Just $ Right $ LinearVec (d, c-1)
  addOne (DiagonalDirection Inwards p) (5, f) = Just (5, pmMod24Oper p 10 f)
  addOne (DiagonalDirection Inwards p) (rank, file) = Just (inw rank, filewiseInc p file)
  addOne (DiagonalDirection Outwards p) (rank, file) =
    do { o <- out rank; return (o, filewiseInc p file)}
class (LinearDirection a) => StraightDirection a where
  orientation :: a -> Orientation
instance StraightDirection RankwiseDirection where
  orientation _ = Rankwise
instance StraightDirection FilewiseDirection where
  orientation _ = Filewise

class (Eq a-- , Read a, Show a
      ) => Vec a where
  reverMaybe :: a -> Maybe a
  add :: Pos -> a -> Maybe Pos
  emptiesFrom :: Pos -> a -> Maybe [Pos]
--  moats :: Pos -> a -> [MoatLocalization]
class (Eq a, Show a -- , Read a
      ) => InterfaceVecEBC a where
  reverMaybeEBC :: a -> Maybe a
  addEBC :: Pos -> a -> Maybe Pos
  flipAddEBC :: a -> Pos -> Maybe Pos
  addEBC = flip flipAddEBC
  flipEmptiesFromEBC :: a -> Pos -> Maybe [Pos]
  emptiesFromEBC :: Pos -> a -> Maybe [Pos]
  emptiesFromEBC = flip flipEmptiesFromEBC
  moats :: Pos -> a -> [MoatLocalization]
--  moatsEBC :: Pos -> a -> [MoatLocalization]
--  flipMoatsEBC :: a -> Pos -> [MoatLocalization]
--  moatsEBC = flip flipMoatsEBC
-- data VecC = forall a . Vec a => MkVecC a
data VecC where
  MkVecC :: Vec a => a -> VecC
data VecCC where
  MkLinearVecCC :: LinearDirection a => LinearVec a -> VecCC
--  MkStraightVecCC :: StraightDirection a => LinearVec a -> VecCC
  MkKnightVecCC :: KnightVec -> VecCC
  MkCastlingVecCC :: Castling -> VecCC
  MkPawnJumpByTwoVecCC :: PawnJumpByTwo -> VecCC

-- unboxVecCCLinear :: (LinearDirection a) => ('MkLinearVecCC (LinearVec a)) -> Maybe (LinearVec a)
-- unboxVecCCLinear (MkLinearVecCC a) = Just a
data VecEBC where
  MkLinearVecEBC :: LinearVecEBC -> VecEBC
  MkKnightVecEBC :: KnightVec -> VecEBC
  MkCastlingVecEBC :: Castling -> VecEBC
  MkPawnJumpByTwoVecEBC :: PawnJumpByTwo -> VecEBC
instance Show VecEBC where
  show (MkLinearVecEBC x) = "an EBC-contained linear vec of " ++ show x
  show (MkKnightVecEBC x) = "an EBC-contained knight vec of " ++ show x
  show (MkCastlingVecEBC x) = "an EBC-contained castling vec of " ++ show x
  show (MkPawnJumpByTwoVecEBC x) = "an EBC-contained pawn jump vec of " ++ show x
data StraightVecEBC where
  MkRankwiseVecEBC :: LinearVec RankwiseDirection -> StraightVecEBC
  MkFilewiseVecEBC :: LinearVec FilewiseDirection -> StraightVecEBC
data LinearVecEBC where
  MkDiagonalVecEBC :: LinearVec DiagonalDirection -> LinearVecEBC
  MkStraightVecEBC :: StraightVecEBC -> LinearVecEBC
instance Show LinearVecEBC where
  show (MkDiagonalVecEBC x) = "an EBC-contained diagonal vec of " ++ show x
  show (MkStraightVecEBC x) = "an EBC-contained straight vec of " ++ show x
instance Show StraightVecEBC where
  show (MkFilewiseVecEBC x) = "an EBC-contained filewise vec of " ++ show x
  show (MkRankwiseVecEBC x) = "an EBC-contained rankwise vec of " ++ show x
instance (Show a, LinearDirection a) => Show (LinearVec a) where
  show (LinearVec (b, c)) = "a LinearVec of count " ++ show c ++ " and direction " ++ show b
instance InterfaceVecEBC StraightVecEBC where
  reverMaybeEBC (MkRankwiseVecEBC x) = MkRankwiseVecEBC <$> reverMaybe x
  reverMaybeEBC (MkFilewiseVecEBC x) = MkFilewiseVecEBC <$> reverMaybe x
  flipAddEBC (MkRankwiseVecEBC x) = flip add x
  flipAddEBC (MkFilewiseVecEBC x) = flip add x
  flipEmptiesFromEBC (MkRankwiseVecEBC x) = flip emptiesFrom x
  flipEmptiesFromEBC (MkFilewiseVecEBC x) = flip emptiesFrom x
  moats _ (MkRankwiseVecEBC _) = []
  moats p (MkFilewiseVecEBC x) = moatsFilewise p x
--  flipMoatsEBC (MkRankwiseVecEBC x) = flip moats x
--  flipMoatsEBC (MkFilewiseVecEBC x) = flip moats x
instance InterfaceVecEBC LinearVecEBC where
  reverMaybeEBC (MkStraightVecEBC x) = MkStraightVecEBC <$> reverMaybeEBC x
  reverMaybeEBC (MkDiagonalVecEBC x) = MkDiagonalVecEBC <$> reverMaybe x
  flipAddEBC (MkStraightVecEBC x) = flipAddEBC x
  flipAddEBC (MkDiagonalVecEBC x) = flip add x
  flipEmptiesFromEBC (MkStraightVecEBC x) = flipEmptiesFromEBC x
  flipEmptiesFromEBC (MkDiagonalVecEBC x) = flip emptiesFrom x
--  flipMoatsEBC (MkStraightVecEBC x) = flip moats x
--  flipMoatsEBC (MkDiagonalVecEBC x) = flip moats x
  moats a (MkDiagonalVecEBC x) = moatsDiagonal a x
  moats a (MkStraightVecEBC x) = moats a x
instance InterfaceVecEBC VecEBC where
  reverMaybeEBC (MkLinearVecEBC x) = MkLinearVecEBC <$> reverMaybeEBC x
  reverMaybeEBC (MkKnightVecEBC x) = MkKnightVecEBC <$> reverMaybe x
  reverMaybeEBC (MkCastlingVecEBC x) = MkCastlingVecEBC <$> reverMaybe x
  reverMaybeEBC (MkPawnJumpByTwoVecEBC x) = MkPawnJumpByTwoVecEBC <$> reverMaybe x
  flipAddEBC (MkLinearVecEBC x) = flipAddEBC x
  flipAddEBC (MkKnightVecEBC x) = flip add x
  flipAddEBC (MkCastlingVecEBC x) = flip add x
  flipAddEBC (MkPawnJumpByTwoVecEBC x) = flip add x
  flipEmptiesFromEBC (MkLinearVecEBC x) = flipEmptiesFromEBC x
  flipEmptiesFromEBC (MkKnightVecEBC x) = flip emptiesFrom x
  flipEmptiesFromEBC (MkCastlingVecEBC x) = flip emptiesFrom x
  flipEmptiesFromEBC (MkPawnJumpByTwoVecEBC x) = flip emptiesFrom x
  moats _ (MkCastlingVecEBC _) = []
  moats _ (MkPawnJumpByTwoVecEBC _) = []
  moats p (MkLinearVecEBC x) = moats p x
  moats p (MkKnightVecEBC x) = moatsKnight p x
  -- flipMoatsEBC (MkLinearVecEBC x) = flip moats x
  -- flipMoatsEBC (MkKnightVecEBC x) = flip moats x
  -- flipMoatsEBC (MkCastlingVecEBC x) = flip moats x
  -- flipMoatsEBC (MkPawnJumpByTwoVecEBC x) = flip moats x
_xrqnmv :: SegmentEight -> SegmentEight -> Maybe (Rank, FilewiseDirection)
_xrqnmv 6 0 = Just (1, Pluswards)
_xrqnmv 7 1 = Just (1, Pluswards)
_xrqnmv 7 0 = Just (2, Pluswards)
_xrqnmv 0 6 = Just (1, Minuswards)
_xrqnmv 0 7 = Just (2, Minuswards)
_xrqnmv 1 7 = Just (1, Minuswards)
_xrqnmv _ _ = Nothing
_xoreq :: Pos -> Pos -> Maybe Bool
_xoreq (fr,fswc) (tr, tswc)
  | tr>2 && fr>2 = Nothing
  | otherwise =
      do (w,_) <- _xrqnmv (segmFile fswc) (segmFile tswc);
         Just $ case fr of
                  0 -> tr==w
                  _ -> fr==w && tr==0
moatKnight :: Pos -> KnightVec -> Maybe MoatLocalization
moatKnight f v = do
  to <- add f v;
  (_,wd) <- _xrqnmv (segmFile (file f)) (segmFile (file to));
  xo <- _xoreq f to;
  if xo then Just (onDirecLoc wd (segmColor (file f))) else Nothing
instance Eq StraightVecEBC where
  (MkRankwiseVecEBC x) == (MkRankwiseVecEBC y) = x==y
  (MkFilewiseVecEBC x) == (MkFilewiseVecEBC y) = x==y
  _ == _ = False
instance Eq LinearVecEBC where
  (MkDiagonalVecEBC x) == (MkDiagonalVecEBC y) = x==y
  (MkStraightVecEBC x) == (MkStraightVecEBC y) = x==y
  _ == _ = False
instance Eq VecEBC where
  (MkLinearVecEBC x) == (MkLinearVecEBC y) = x==y
  (MkKnightVecEBC x) == (MkKnightVecEBC y) = x==y
  (MkCastlingVecEBC x) == (MkCastlingVecEBC y) = x==y
  (MkPawnJumpByTwoVecEBC x) == (MkPawnJumpByTwoVecEBC y) = x==y
  _ == _ = False

class (Vec a, Reversable a) => ReversableVec a
-- instance (Reversable a) => Vec (ReversableVec a) where
--   reverMaybe x = Just $ rever x
{-@ passTimes :: (a -> a) -> PosInt -> a -> a @-}
passTimes :: (a -> a) -> Int -> a -> a
passTimes f 1 a = f a
passTimes f c a = passTimes f (c-1) (f a)
data Castling = QueensideCastling | KingsideCastling deriving Show -- deriving (Vec)
rookGoesInCastling :: Castling -> SegmentEight
rookGoesInCastling ca = fromJust $ (case ca of
                                      KingsideCastling -> plusEight
                                      QueensideCastling -> minusEight) kfm
rookTo :: Castling -> Color -> Pos
rookTo ca co = (0, fileFromColorAndSegmFile (co, rookGoesInCastling ca))
rookFrom :: Castling -> Color -> Pos
rookFrom QueensideCastling co = (0, fileFromColorAndSegmFile (co, 0))
rookFrom KingsideCastling co = (0, fileFromColorAndSegmFile (co, 7))
emptiesForCastling :: Castling -> [SegmentEight]
emptiesForCastling KingsideCastling = [fromJust $ plusEight kfm, passTimes (fromJust.plusEight) 2 kfm]
emptiesForCastling QueensideCastling = [fromJust $ minusEight kfm, passTimes (fromJust.minusEight) 2 kfm,
                                        passTimes (fromJust.minusEight) 3 kfm]
instance Eq Castling where
  QueensideCastling == QueensideCastling = True
  QueensideCastling == KingsideCastling = False
  KingsideCastling == QueensideCastling = False
  KingsideCastling == KingsideCastling = True
instance Vec Castling where
  reverMaybe _ = Nothing
  add (r, f) cas
    | r==0 && segmFile f == 4 = Just 
        (0, 
         fileFromColorAndSegmFile 
          (segmColor f, 
            case cas of
              QueensideCastling -> 2
              KingsideCastling -> 6))
    | otherwise = undefined
  emptiesFrom from v = Just $ assert ( segmFile (file from) == kfm )
                       (assert ( rank from == 0 ) (fmap (\x -> (0, fileFromColorAndSegmFile (segmColor $ file from, x)))
                                                        (emptiesForCastling v)))
data PawnJumpByTwo = PawnJumpByTwo deriving Show --deriving (Vec)
enPassantField :: File -> Pos
enPassantField f = (2, f)
enPassantFieldPos :: Pos -> Maybe Pos
enPassantFieldPos (1,f) = Just $ enPassantField f
enPassantFieldPos _ = Nothing
instance Eq PawnJumpByTwo where
  PawnJumpByTwo == PawnJumpByTwo = True
instance Vec PawnJumpByTwo where
  reverMaybe PawnJumpByTwo = Nothing
  add (1, f) PawnJumpByTwo = Just (3, f)
  add _ PawnJumpByTwo = Nothing
  emptiesFrom from PawnJumpByTwo = do { e <- enPassantFieldPos from; t <- add from PawnJumpByTwo; Just [e, t] }
-- data (LinearDirection a) => LinearVec a = LinearVec a Count --deriving (Ord)
{-@ newtype LinearVec a = LinearVec (LinearDirection a => (a, PosInt)) @-}
newtype LinearVec a = LinearVec (LinearDirection a => (a, Int))
-- data LinearVec a where
--  LinearVec :: LinearDirection a => a -> Int -> LinearVec a
data LinearVecC = forall a . LinearDirection a => MkLinearVecC (LinearVec a)
-- data LinearVecC where
--   MkLinearVecC :: LinearDirection a => a -> LinearVecC (LinearVec a)
data StraightVecC = forall a . StraightDirection a => MkStraightVecC (LinearVec a)
-- data StraightVecC where
--   MkStraightVecC :: StraightDirection a => a -> StraightVecC (LinearVec a)
direction :: (LinearDirection a) => LinearVec a -> a
direction (LinearVec (d, _)) = d
{-@ count :: (LinearDirection a) => LinearVec a -> PosInt @-}
count :: (LinearDirection a) => LinearVec a -> Int
count (LinearVec (_, c)) = c
-- instance (LinearDirection a) => Ord (LinearVec a)
instance (LinearDirection a) => ReversableVec (LinearVec a)
instance (LinearDirection a) => Reversable (LinearVec a) where
  rever (LinearVec (d, n)) = LinearVec (rever d, n)
instance (LinearDirection t) => Eq (LinearVec t) where
  (LinearVec (a, c)) == (LinearVec (b, d)) = (a == b) && (c == d)
instance (LinearDirection a) => Vec (LinearVec a) where
  reverMaybe x = Just $ rever x
  add p (LinearVec (d, 1)) = addOne d p
  add p m = foldl _addMaybe (Just p) (unitsInvolRank m (rank p))
  emptiesFrom _ (LinearVec (_, 1)) = Just []
  emptiesFrom p (LinearVec (d, c)) =  addOne d p >>= (\pp -> Just $ pp:_emptiesFromMust pp (LinearVec (d, c)))

zeroSegmentEight :: SegmentEight
zeroSegmentEight = 0
sevenSegmentEight :: SegmentEight
sevenSegmentEight = 7
moatFilewise :: Pos -> FilewiseDirection -> Maybe MoatLocalization
moatFilewise (0, f) d
  | (segmFile f)==0 && d==Minuswards = Just $ onDirecLoc d (segmColor f)
  | (segmFile f)==7 && d==Pluswards = Just $ onDirecLoc d (segmColor f)
  | otherwise = Nothing
moatFilewise _ _ = Nothing
moatsFilewise :: Pos -> LinearVec FilewiseDirection -> [MoatLocalization]
moatsFilewise p (LinearVec (d, 1)) = maybeToList $ moatFilewise p d
moatsFilewise p (LinearVec (d, c)) = maybeToList (moatFilewise p d) ++ moatsFilewise p (LinearVec (d, c-1))
moatsDiagonal :: Pos -> LinearVec DiagonalDirection -> [MoatLocalization]
moatsDiagonal p x = maybeToList $ moatDiagonal p x
moatsKnight :: Pos -> KnightVec -> [MoatLocalization]
moatsKnight p x = maybeToList $ moatKnight p x

moatDiagonal :: Pos -> LinearVec DiagonalDirection -> Maybe MoatLocalization
moatDiagonal (0, f) (LinearVec (DiagonalDirection Inwards p, _))
  | (segmFile f)==7 && p==Pluswards = Just $ onDirecLoc p (segmColor f)
  | (segmFile f)==0 && p==Minuswards = Just $ onDirecLoc p (segmColor f)
  | otherwise = Nothing
moatDiagonal f (LinearVec (DiagonalDirection Outwards Pluswards, c)) =
  do tove <- add f (LinearVec (DiagonalDirection Outwards Pluswards, c));
     if rank tove == 0 then
       case segmFile (file tove) of
         0 -> Just $ onDirecLoc Minuswards (segmColor (file tove))
         _ -> Nothing
     else Nothing
moatDiagonal f (LinearVec (DiagonalDirection Outwards Minuswards, c)) =
  do tove <- add f (LinearVec (DiagonalDirection Outwards Minuswards, c));
     if rank tove == 0 then
       case segmFile (file tove) of
         7 -> Just $ onDirecLoc Pluswards (segmColor (file tove))
         _ -> Nothing
     else Nothing
moatDiagonal _ _ = Nothing
_emptiesFromMust :: (LinearDirection a) => Pos -> LinearVec a -> [Pos]
_emptiesFromMust _ (LinearVec (_, 1)) = []
_emptiesFromMust pp (LinearVec (d, c)) = fromJust $ emptiesFrom pp (fromJust (tailInvolRank $ LinearVec (d, c)) (rank pp))
_addMaybe :: (LinearDirection a) => Maybe Pos -> LinearVec a -> Maybe Pos
_addMaybe p m = do { jp <- p;
                     add jp m;}
data KnightVec = KnightVec RankwiseDirection FilewiseDirection Orientation deriving Show -- Orientation :: twice
-- instance Ord KnightVec
instance Eq KnightVec where
  (KnightVec r f o) == (KnightVec rr ff oo) = (r==rr)&&(f==ff)&&(o==oo)
instance ReversableVec KnightVec
instance Reversable KnightVec where
  rever (KnightVec r f orient) = KnightVec (rever r) (rever f) orient
instance Vec KnightVec where
  emptiesFrom _ _ = Just []
  reverMaybe x = Just $ rever x
-- add KnightMove {rankwise=r, filewise=f, twice=t} x =
--   do { fl <- add UnitStraightMove {direction=case t of Rankwise -> r; Filewise -> f} x;
--        return add UnitStraightMove {direction=case t of Filewise -> r; Rankwise -> f} fl }
  add x (KnightVec r f t) =
    do { x <- addOne r x;
         x <- addOne f x;
         x <- (case t of Rankwise -> addOne r; Filewise -> addOne f) x;
         return x;}

allPossibleKnightVecs :: [KnightVec]
allPossibleKnightVecs = [KnightVec Inwards Pluswards Rankwise, KnightVec Inwards Pluswards Filewise,
                         KnightVec Inwards Minuswards Rankwise, KnightVec Inwards Minuswards Filewise,
                         KnightVec Outwards Pluswards Rankwise, KnightVec Outwards Pluswards Filewise,
                         KnightVec Outwards Minuswards Rankwise, KnightVec Outwards Minuswards Filewise]
fromToKnight :: Pos -> Pos -> [KnightVec]
fromToKnight a b = filter (maybe False (b==) . add a) allPossibleKnightVecs
-- add LinearMove m p = foldl add p $ unitsInvolRank m
-- class StraightVec
-- class (StraightDirection a) => StraightVec a
-- instance (StraightDirection a) => StraightVec (LinearVec a)
-- type (StraightDirection a) => StraightVec a = LinearVec a

mkveccFromLinearvecc :: LinearVecC -> VecC
mkveccFromLinearvecc (MkLinearVecC a) = MkVecC a
-- linearVecUnboxingFromVecc :: (LinearDirection a) => VecC -> Maybe (LinearVec a)
-- linearVecUnboxingFromVecc (MkVecC (LinearVec a b)) = Just $ LinearVec a b
-- mklinearveccFromVecc :: (Linear)
-- mklinearveccFromVecc (MkVecC x) = MkLinearVecC x

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
headLinear (LinearVec (d, _)) = LinearVec (d, 1)
tailFilewise :: LinearVec FilewiseDirection -> Maybe (LinearVec FilewiseDirection)
tailFilewise (LinearVec (_, 1)) = Nothing
tailFilewise (LinearVec (d, c)) = Just $ LinearVec (d, c-1)
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
tailInvolRank :: (LinearDirection a) => LinearVec a -> Maybe (Rank -> LinearVec a)
-- tailInvolRank x = do { f <- tailRankInvol x; return $ either id const f }
tailInvolRank x = either id const <$> tailRankInvol x
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
                   Nothing -> LinearVec (d, 1)
                   Just (LinearVec (d, om)) ->  LinearVec (d, om+1)
fromToRankwise :: Pos -> Pos -> [LinearVec RankwiseDirection]
fromToRankwise (a, b) (c, d)
  | b == d = maybeToList (fromToRanks (a,c))
  | opposite b == d = maybeToList $ do { t <- (case a of
                                                5 -> Just $ LinearVec (Inwards, 1)
                                                a -> fromToRanks (a,5));
                                         o <- fromToRanks (5,c);
                                         return $ LinearVec (Inwards, (count t) + (count o)) }
  | otherwise = []
{-@ fromToFilesWards :: FilewiseDirection -> File -> File -> NNegInt @-}
fromToFilesWards :: FilewiseDirection -> File -> File -> Int
fromToFilesWards w a b | a==b = 0
  | otherwise = let wf = filewiseInc w in
  let c = wf a in
    if c==b then 1
    else (fromToFilesWards w c b) + 1
fromToFiles :: (File, File) -> Maybe (LinearVec FilewiseDirection, LinearVec FilewiseDirection)
fromToFiles (a,b) = do { p <- maybePosInt $ fromToFilesWards Pluswards a b;
                         m <- maybePosInt $ fromToFilesWards Minuswards a b;
                         return $ let { pv = LinearVec (Pluswards, p);
                                        mv = LinearVec (Minuswards, m) } in
                                    if m<p then (mv,pv) else (pv,mv) }
fromToFilewise :: Pos -> Pos -> [LinearVec FilewiseDirection]
fromToFilewise (a, b) (c, d)
  | a == c = fromMaybe [] $ do { f <- fromToFiles (b,d);
                                 return [fst f, snd f] }
  | otherwise = []
-- type StraightVecsOfKinds = ([LinearVec RankwiseDirection], [LinearVec FilewiseDirection])
-- type LinearVecsOfKinds = (StraightVecsOfKinds, [LinearVec DiagonalDirection])
-- fromToStraight :: Pos -> Pos -> StraightVecsOfKinds
fromToStraight :: Pos -> Pos -> [StraightVecEBC]
fromToStraight a b = (map MkRankwiseVecEBC (fromToRankwise a b)) ++ (map MkFilewiseVecEBC (fromToFilewise a b))
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
filewiseToShortDiagonal :: LinearVec FilewiseDirection -> RankwiseDirection -> LinearVec DiagonalDirection
filewiseToShortDiagonal (LinearVec (f, c)) r = LinearVec (DiagonalDirection r f, c)
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
filewiseToLongDiagonal (LinearVec (f, fileCount)) = LinearVec (DiagonalDirection Inwards (rever f), 5+5+1-fileCount)
{-@ fileDistance :: (File,File) -> NNegInt @-}
fileDistance :: (File,File) -> Int
fileDistance = maybe 0 count . fromToFilesShort
{-@ fileCoorDistance :: (Pos,Pos) -> NNegInt @-}
fileCoorDistance :: (Pos,Pos) -> Int
fileCoorDistance = fileDistance . mapHomoTuple2 file
fromToCoorFilesShort :: (Pos,Pos) -> Maybe (LinearVec FilewiseDirection)
fromToCoorFilesShort = fromToFilesShort . mapHomoTuple2 file
fromToCoorRanks :: (Pos,Pos) -> Maybe (LinearVec RankwiseDirection)
fromToCoorRanks = fromToRanks. mapHomoTuple2 rank
{-@ rankDistance :: (Rank, Rank) -> NNegInt @-}
rankDistance :: (Rank, Rank) -> Int
rankDistance = maybe 0 count . fromToRanks
{-@ rankDistanceFromMostOuter :: Rank -> NNegInt @-}
rankDistanceFromMostOuter :: Rank -> Int
rankDistanceFromMostOuter = maybe 0 count . curry fromToRanks 0
{-@ sumOfRankDistancesFromMostOuter :: (Rank, Rank) -> NNegInt @-}
sumOfRankDistancesFromMostOuter :: (Rank, Rank) -> Int
sumOfRankDistancesFromMostOuter =
  (uncurry (+)) . mapHomoTuple2 rankDistanceFromMostOuter
{-@ sumOfRankCoorDistancesFromMostOuter :: (Pos,Pos) -> NNegInt @-}
sumOfRankCoorDistancesFromMostOuter :: (Pos,Pos) -> Int
sumOfRankCoorDistancesFromMostOuter =
  (uncurry (+)) . mapHomoTuple2 (rankDistanceFromMostOuter.rank)
isFilewiseDistanceSameAsSumOfRanksDistancesFromMostOuter :: (Pos,Pos) -> Bool
isFilewiseDistanceSameAsSumOfRanksDistancesFromMostOuter x =
  (fileCoorDistance x) == (sumOfRankCoorDistancesFromMostOuter x)
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
fromToLinear :: Pos -> Pos -> [LinearVecEBC]
fromToLinear a b = fmap MkStraightVecEBC (fromToStraight a b) ++
                   fmap MkDiagonalVecEBC (fromToDiagonal a b)

data ShortOrLong = Short | Long
data CanIDiagonal = CanDiagonalBut ShortOrLong FilewiseDirection | CanDiagonalBoth FilewiseDirection | CannotDiagonal
