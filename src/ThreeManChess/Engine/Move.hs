{-# LANGUAGE DatatypeContexts, TypeFamilies, DataKinds, ExistentialQuantification, GADTs, DuplicateRecordFields #-}

module ThreeManChess.Engine.Move where

import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.Figure
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.Possibilities
import ThreeManChess.Engine.FigType
import ThreeManChess.Engine.GameState
import ThreeManChess.Engine.GameBoard
import ThreeManChess.Engine.PlayersAlive
import ThreeManChess.Engine.Board
import ThreeManChess.Engine.Moats
import ThreeManChess.Engine.CastlingPossibilities
import ThreeManChess.Engine.EnPassantStore
import ThreeManChess.Engine.Directions
import Data.Maybe

data (Vec a) => BoundVec a = BoundVec a Pos

type AloneKingMove = LinearDirecEBC
-- data KingMove = AloneDiagonally DiagonalDirection | AloneRankwise RankwiseDirection | AloneFilewise FilewiseDirection | NotAlone Castling deriving (Eq, Show)
data KingMove = Alone AloneKingMove | NotAlone Castling deriving (Eq, Show)
data JustPawnMove = Forward | Capturing FilewiseDirection deriving (Eq, Show)
data PossiblyJumpingPawnMove = Walk JustPawnMove | Jump deriving (Eq, Show)
-- data Promotion = forall a . PromotionDesire a => Promotion FigType
-- data Promotion where
--   Promotion :: PromotionDesire a => a -> Promotion
type PossiblyPromotedPawnMove = (JustPawnMove, Maybe Promotion)

type family Move (f :: FigType) where
  Move 'Queen = LinearVecEBC
  Move 'King = KingMove
  Move 'Rook = StraightVecEBC
  Move 'Bishop = LinearVec DiagonalDirection
  Move 'Knight = KnightVec
  Move 'InwardPawn = PossiblyJumpingPawnMove
  Move 'OutwardPawn = PossiblyPromotedPawnMove

vectorFromMoveT :: MoveT -> VecEBC
vectorFromMoveT (MkQueenMove x) = MkLinearVecEBC x
vectorFromMoveT (MkKingMove (Alone (MkDiagonalDirecEBC x))) = MkLinearVecEBC $ MkDiagonalVecEBC $ LinearVec x Once
vectorFromMoveT (MkKingMove (Alone (MkStraightDirecEBC (MkRankwiseDirecEBC x)))) = MkLinearVecEBC $ MkStraightVecEBC $ MkRankwiseVecEBC $ LinearVec x Once
vectorFromMoveT (MkKingMove (Alone (MkStraightDirecEBC (MkFilewiseDirecEBC x)))) = MkLinearVecEBC $ MkStraightVecEBC $ MkFilewiseVecEBC $ LinearVec x Once
vectorFromMoveT (MkKingMove (NotAlone x)) = MkCastlingVecEBC x
vectorFromMoveT (MkRookMove x) = MkLinearVecEBC $ MkStraightVecEBC x
vectorFromMoveT (MkBishopMove x) = MkLinearVecEBC $ MkDiagonalVecEBC x
vectorFromMoveT (MkKnightMove x) = MkKnightVecEBC x
vectorFromMoveT (MkInwardPawnMove (Walk Forward)) = MkLinearVecEBC $ MkStraightVecEBC $ MkRankwiseVecEBC $ LinearVec Inwards Once
vectorFromMoveT (MkInwardPawnMove (Walk (Capturing x))) = MkLinearVecEBC $ MkDiagonalVecEBC $ LinearVec (DiagonalDirection Inwards x) Once
vectorFromMoveT (MkInwardPawnMove Jump) = MkPawnJumpByTwoVecEBC PawnJumpByTwo
vectorFromMoveT (MkOutwardPawnMove (Forward,_)) = MkLinearVecEBC $ MkStraightVecEBC $ MkRankwiseVecEBC $ LinearVec Outwards Once
vectorFromMoveT (MkOutwardPawnMove (Capturing x,_)) = MkLinearVecEBC $ MkDiagonalVecEBC $ LinearVec (DiagonalDirection Outwards x) Once
vectorFromPossiblyPromotedMoveTNoEither :: (Maybe Promotion -> MoveT) -> VecEBC
vectorFromPossiblyPromotedMoveTNoEither f = vectorFromMoveT (f Nothing)
vectorFromPossiblyPromotedMoveT :: Either (Maybe Promotion -> MoveT) MoveT -> VecEBC
vectorFromPossiblyPromotedMoveT = either vectorFromPossiblyPromotedMoveTNoEither vectorFromMoveT
disregardPromotionPossibOfEither :: Either (Maybe Promotion -> MoveT) MoveT -> MoveT
disregardPromotionPossibOfEither (Right x) = x
disregardPromotionPossibOfEither (Left f) = f Nothing
vectorFromHypoCapMoveT :: HypoCapMoveT -> VecEBC
vectorFromHypoCapMoveT = vectorFromMoveT.disregardPromotionPossibOfEither.hypoMoveToNormalMove

vecsFromToWith :: FigType -> Pos -> Pos -> Color -> [VecEBC]
vecsFromToWith Queen a b _ = fmap MkLinearVecEBC (fromToLinear a b)
vecsFromToWith King a b col
  | maybe False (b==) (addOne (DiagonalDirection Inwards Pluswards) a) =
    [MkLinearVecEBC $ MkDiagonalVecEBC $ flip LinearVec Once $ DiagonalDirection Inwards Pluswards]
  | maybe False (b==) (addOne (DiagonalDirection Inwards Minuswards) a) =
    [MkLinearVecEBC $ MkDiagonalVecEBC $ flip LinearVec Once $ DiagonalDirection Inwards Minuswards]
  | maybe False (b==) (addOne (DiagonalDirection Outwards Pluswards) a) =
    [MkLinearVecEBC $ MkDiagonalVecEBC $ flip LinearVec Once $ DiagonalDirection Outwards Pluswards]
  | maybe False (b==) (addOne (DiagonalDirection Outwards Minuswards) a) =
    [MkLinearVecEBC $ MkDiagonalVecEBC $ flip LinearVec Once $ DiagonalDirection Outwards Minuswards]
  | maybe False (b==) (addOne Pluswards a) = [MkLinearVecEBC $ MkStraightVecEBC $ MkFilewiseVecEBC $ LinearVec Pluswards Once]
  | maybe False (b==) (addOne Minuswards a) = [MkLinearVecEBC $ MkStraightVecEBC $ MkFilewiseVecEBC $ LinearVec Minuswards Once]
  | maybe False (b==) (addOne Inwards a) = [MkLinearVecEBC $ MkStraightVecEBC $ MkRankwiseVecEBC $ LinearVec Inwards Once]
  | maybe False (b==) (addOne Outwards a) = [MkLinearVecEBC $ MkStraightVecEBC $ MkRankwiseVecEBC $ LinearVec Outwards Once]
  | a==(MostOuter, File col kfm) && maybe False (b==) (add a QueensideCastling) = [MkCastlingVecEBC QueensideCastling]
  | a==(MostOuter, File col kfm) && maybe False (b==) (add a KingsideCastling) = [MkCastlingVecEBC KingsideCastling]
  | otherwise = []
vecsFromToWith Rook a b _ = fmap (MkLinearVecEBC . MkStraightVecEBC) (fromToStraight a b)
vecsFromToWith Bishop a b _ = MkLinearVecEBC . MkDiagonalVecEBC <$> fromToDiagonal a b
vecsFromToWith Knight a b _ = MkKnightVecEBC <$> fromToKnight a b
vecsFromToWith InwardPawn a b col
  | maybe False (b==) (addOne Inwards a) = [MkLinearVecEBC $ MkStraightVecEBC $ MkRankwiseVecEBC $ LinearVec Inwards Once]
  | maybe False (b==) (addOne (DiagonalDirection Inwards Pluswards) a) = [MkLinearVecEBC $ MkDiagonalVecEBC $ LinearVec (DiagonalDirection Inwards Pluswards) Once]
  | maybe False (b==) (addOne (DiagonalDirection Inwards Minuswards) a) = [MkLinearVecEBC $ MkDiagonalVecEBC $ LinearVec (DiagonalDirection Inwards Minuswards) Once]
  | rank a ==SecondOuter && segmColor (file a) == col = [MkPawnJumpByTwoVecEBC PawnJumpByTwo]
  | otherwise = []
vecsFromToWith OutwardPawn a b _
  | maybe False (b==) (addOne Outwards a) = [MkLinearVecEBC $ MkStraightVecEBC $ MkRankwiseVecEBC $ LinearVec Outwards Once]
  | maybe False (b==) (addOne (DiagonalDirection Outwards Pluswards) a) =
    [MkLinearVecEBC $ MkDiagonalVecEBC $ LinearVec (DiagonalDirection Outwards Pluswards) Once]
  | maybe False (b==) (addOne (DiagonalDirection Outwards Minuswards) a) =
    [MkLinearVecEBC $ MkDiagonalVecEBC $ LinearVec (DiagonalDirection Outwards Minuswards) Once]
  | otherwise = []

-- moveFromVecWith :: FigType -> VecC -> Either (Maybe Promotion -> MoveT) MoveT
-- moveFromVecWith Queen (MkVecC (LinearVec a b)) = Right $ MkQueenMove (MkLinearVecC a b)
moveFromVecWith :: FigType -> VecEBC -> Maybe (Either (Maybe Promotion -> MoveT) MoveT)
moveFromVecWith Queen (MkLinearVecEBC x) = Just $ Right $ MkQueenMove x
moveFromVecWith Queen _ = Nothing
moveFromVecWith King (MkLinearVecEBC (MkDiagonalVecEBC (LinearVec a Once))) = Just $ Right $ MkKingMove $ Alone $ MkDiagonalDirecEBC a
moveFromVecWith King (MkLinearVecEBC (MkStraightVecEBC (MkRankwiseVecEBC (LinearVec a Once)))) =
  Just $ Right $ MkKingMove $ Alone $ MkStraightDirecEBC $ MkRankwiseDirecEBC a
moveFromVecWith King (MkLinearVecEBC (MkStraightVecEBC (MkFilewiseVecEBC (LinearVec a Once)))) =
  Just $ Right $ MkKingMove $ Alone $ MkStraightDirecEBC $ MkFilewiseDirecEBC a
moveFromVecWith King (MkCastlingVecEBC a) = Just $ Right $ MkKingMove $ NotAlone a
moveFromVecWith King _ = Nothing
moveFromVecWith Rook (MkLinearVecEBC (MkStraightVecEBC a)) = Just $ Right $ MkRookMove a
moveFromVecWith Rook _ = Nothing
moveFromVecWith Knight (MkKnightVecEBC a) = Just $ Right $ MkKnightMove a
moveFromVecWith Knight _ = Nothing
moveFromVecWith Bishop (MkLinearVecEBC (MkDiagonalVecEBC a)) = Just $ Right $ MkBishopMove a
moveFromVecWith Bishop _ = Nothing
moveFromVecWith InwardPawn (MkLinearVecEBC (MkStraightVecEBC (MkRankwiseVecEBC (LinearVec Inwards Once)))) = Just $ Right $ MkInwardPawnMove $ Walk Forward
moveFromVecWith InwardPawn (MkLinearVecEBC (MkDiagonalVecEBC (LinearVec (DiagonalDirection Inwards a) Once))) = Just $ Right $ MkInwardPawnMove $ Walk $ Capturing a
moveFromVecWith InwardPawn (MkPawnJumpByTwoVecEBC PawnJumpByTwo) = Just $ Right $ MkInwardPawnMove Jump
moveFromVecWith InwardPawn _ = Nothing
moveFromVecWith OutwardPawn (MkLinearVecEBC (MkStraightVecEBC (MkRankwiseVecEBC (LinearVec Outwards Once)))) = Just $ Left (\x -> MkOutwardPawnMove (Forward, x))
moveFromVecWith OutwardPawn (MkLinearVecEBC (MkDiagonalVecEBC (LinearVec (DiagonalDirection Outwards a) Once))) =
  Just $ Left (\x -> MkOutwardPawnMove (Capturing a, x))
moveFromVecWith OutwardPawn _ = Nothing
hypoMoveFromVecWith :: FigType -> VecEBC -> Maybe HypoCapMoveT
hypoMoveFromVecWith Queen (MkLinearVecEBC x) = Just $ HypoQueenMove x
hypoMoveFromVecWith Queen _  = Nothing
hypoMoveFromVecWith King (MkLinearVecEBC (MkDiagonalVecEBC (LinearVec a Once))) = Just $ HypoKingMove $ MkDiagonalDirecEBC a
hypoMoveFromVecWith King (MkLinearVecEBC (MkStraightVecEBC (MkRankwiseVecEBC (LinearVec a Once)))) = Just $ HypoKingMove $ MkStraightDirecEBC $ MkRankwiseDirecEBC a
hypoMoveFromVecWith King (MkLinearVecEBC (MkStraightVecEBC (MkFilewiseVecEBC (LinearVec a Once)))) = Just $ HypoKingMove $ MkStraightDirecEBC $ MkFilewiseDirecEBC a
hypoMoveFromVecWith King _ = Nothing
hypoMoveFromVecWith Rook (MkLinearVecEBC (MkStraightVecEBC a)) = Just $ HypoRookMove a
hypoMoveFromVecWith Rook _ = Nothing
hypoMoveFromVecWith Knight (MkKnightVecEBC a) = Just $ HypoKnightMove a
hypoMoveFromVecWith Knight _ = Nothing
hypoMoveFromVecWith Bishop (MkLinearVecEBC (MkDiagonalVecEBC a)) = Just $ HypoBishopMove a
hypoMoveFromVecWith Bishop _ = Nothing
hypoMoveFromVecWith InwardPawn (MkLinearVecEBC (MkDiagonalVecEBC (LinearVec (DiagonalDirection Inwards a) Once))) = Just $ HypoInwardPawnMove a
hypoMoveFromVecWith InwardPawn _ = Nothing
hypoMoveFromVecWith OutwardPawn (MkLinearVecEBC (MkDiagonalVecEBC (LinearVec (DiagonalDirection Outwards a) Once))) = Just $ HypoOutwardPawnMove a
hypoMoveFromVecWith OutwardPawn _ = Nothing
hypoMoveToNormalMove :: HypoCapMoveT -> Either (Maybe Promotion -> MoveT) MoveT
hypoMoveToNormalMove (HypoQueenMove x) = Right $ MkQueenMove x
hypoMoveToNormalMove (HypoKingMove x) = Right $ MkKingMove $ Alone x
hypoMoveToNormalMove (HypoRookMove x) = Right $ MkRookMove x
hypoMoveToNormalMove (HypoBishopMove x) = Right $ MkBishopMove x
hypoMoveToNormalMove (HypoKnightMove x) = Right $ MkKnightMove x
hypoMoveToNormalMove (HypoInwardPawnMove x) = Right $ MkInwardPawnMove $ Walk $ Capturing x
hypoMoveToNormalMove (HypoOutwardPawnMove x) = Left (\p -> MkOutwardPawnMove (Capturing x, p))

movesFromToWith :: FigType -> Pos -> Pos -> Color -> [Either (Maybe Promotion -> MoveT) MoveT]
movesFromToWith ft f t c = fmap (fromJust . moveFromVecWith ft) (vecsFromToWith ft f t c)

hypoMovesFromToWith :: FigType -> Pos -> Pos -> [HypoCapMoveT]
hypoMovesFromToWith ft f t = fmap fromJust $ filter isJust $ fmap (hypoMoveFromVecWith ft) (vecsFromToWith ft f t White)

boundMovePromotionHelper :: Pos -> Either (Maybe Promotion -> MoveT) MoveT -> Either (Maybe Promotion -> BoundMoveT) BoundMoveT
boundMovePromotionHelper (SecondOuter, f) (Left x) = Left (\y -> (x y, (SecondOuter, f)))
boundMovePromotionHelper fr (Left x) = Right (x Nothing, fr)
boundMovePromotionHelper fr (Right x) = Right (x, fr)

boundMovesFromToWith :: FigType -> Pos -> Pos -> Color -> [Either (Maybe Promotion -> BoundMoveT) BoundMoveT]
boundMovesFromToWith ft f t c = boundMovePromotionHelper f <$> movesFromToWith ft f t c

data MoveT where
  MkQueenMove :: Move 'Queen -> MoveT
  MkKingMove :: Move 'King -> MoveT
  MkRookMove :: Move 'Rook -> MoveT
  MkBishopMove :: Move 'Bishop -> MoveT
  MkKnightMove :: Move 'Knight -> MoveT
  MkInwardPawnMove :: Move 'InwardPawn -> MoveT
  MkOutwardPawnMove :: Move 'OutwardPawn -> MoveT

data HypoCapMoveT where
  HypoQueenMove :: Move 'Queen -> HypoCapMoveT
  HypoKingMove :: AloneKingMove -> HypoCapMoveT
  HypoRookMove :: Move 'Rook -> HypoCapMoveT
  HypoBishopMove :: Move 'Bishop -> HypoCapMoveT
  HypoKnightMove :: Move 'Knight -> HypoCapMoveT
  HypoInwardPawnMove :: FilewiseDirection -> HypoCapMoveT
  HypoOutwardPawnMove :: FilewiseDirection -> HypoCapMoveT

type BoundHypoCapMoveT = (HypoCapMoveT, Pos)
data HypoStateMove = HypoStateMove {hypoMove :: BoundHypoCapMoveT, hypoBefore :: HypoCheckState}

instance Eq MoveT where
  MkQueenMove x == MkQueenMove y = x==y
  MkKingMove x == MkKingMove y = x==y
  MkRookMove x == MkRookMove y = x==y
  MkBishopMove x == MkBishopMove y = x==y
  MkKnightMove x == MkKnightMove y = x==y
  MkInwardPawnMove x == MkInwardPawnMove y = x==y
  MkOutwardPawnMove x == MkOutwardPawnMove y = x==y
  _ == _ = False

instance Show MoveT where
  show (MkQueenMove x) = "A Contained Queen Move of " ++ show x
  show (MkKingMove x) = "A Contained King Move of " ++ show x
  show (MkRookMove x) = "A Contained Rook Move of " ++ show x
  show (MkBishopMove x) = "A Contained Bishop Move of " ++ show x
  show (MkKnightMove x) = "A Contained Knight Move of " ++ show x
  show (MkInwardPawnMove x) = "A Contained InwardPawn Move of " ++ show x
  show (MkOutwardPawnMove x) = "A Contained OutwardPawn Move of " ++ show x

-- encapsulateMoveType :: FigType -> Move f -> MoveT
-- encapsulateMoveType Queen x = MkQueenMove x
-- encapsulateMoveType King x = MkKnightMove x
-- encapsulateMoveType Rook x = MkRookMove x
-- encapsulateMoveType Bishop x = MkBishopMove x
-- encapsulateMoveType Knight x = MkKnightMove x
-- encapsulateMoveType InwardPawn x = MkInwardPawnMove x
-- encapsulateMoveType OutwardPawn x = MkOutwardPawnMove x

type BoundMove f = (Move f, Pos)
type BoundMoveT = (MoveT, Pos)
from :: (a,Pos) -> Pos
from = snd
to :: BoundMoveT -> Maybe Pos
to (m,f) = addEBC f $ vectorFromMoveT m
hTo :: BoundHypoCapMoveT -> Maybe Pos
hTo (m,f) = addEBC f $ vectorFromMoveT $ disregardPromotionPossibOfEither $ hypoMoveToNormalMove m
checkIfPromotionPresenceIsOKforOP :: BoundMove 'OutwardPawn -> Bool
checkIfPromotionPresenceIsOKforOP ((_,Just _),(SecondOuter,_)) = True
checkIfPromotionPresenceIsOKforOP ((_,Nothing),_) = True
checkIfPromotionPresenceIsOKforOP _ = False
checkIfPromotionPresenceIsOK :: BoundMoveT -> Bool
checkIfPromotionPresenceIsOK (MkOutwardPawnMove x,f) = checkIfPromotionPresenceIsOKforOP (x,f)
checkIfPromotionPresenceIsOK _ = True

-- boardSimplyAfter :: GameBoard -> BoundMoveT

data StateMove = StateMove {move :: BoundMoveT, before :: GameState}

boardSimplyAfter :: StateMove -> Maybe GameBoard
boardSimplyAfter sm = (\x -> x (board (before sm))) . performSingleChange <$> boardOperation sm
boardOperation :: StateMove -> Maybe GameBoardSingleChange
boardOperation StateMove{move=(mo, po), before=be} =
  case mo of
    (MkQueenMove _) -> do tosm <- to (mo,po); Just $ MoveFromToOverwriting po tosm
    (MkKnightMove _) -> do tosm <- to (mo,po); Just $ MoveFromToOverwriting po tosm
    (MkRookMove _) -> do tosm <- to (mo,po); Just $ MoveFromToOverwriting po tosm
    (MkBishopMove _) -> do tosm <- to (mo,po); Just $ MoveFromToOverwriting po tosm
    (MkKingMove (Alone _)) -> do tosm <- to (mo, po); Just $ MoveFromToOverwriting po tosm
    (MkKingMove (NotAlone x)) -> do
      tosm <- to (mo,po);
      whh <- whoMove (StateMove (mo,po) be);
      Just $ DoubleMoveFromToOverwriting (po,tosm) (rookFrom x whh, rookTo x whh)
    (MkInwardPawnMove (Walk Forward)) -> do tosm <- to (mo, po); Just $ MoveFromToOverwriting po tosm
    (MkInwardPawnMove Jump) -> do tosm <- to (mo, po); Just $ MoveFromToOverwriting po tosm
    (MkOutwardPawnMove (Forward,Nothing)) -> do tosm <- to (mo, po); Just $ MoveFromToOverwriting po tosm
    (MkOutwardPawnMove (_,Just a)) -> do
      tosm <- to (mo,po);
      whh <- whoMove (StateMove (mo,po) be);
      Just $ MoveWithReplacement (po,tosm) (Figure (desiredType a) whh)
    (MkInwardPawnMove (Walk (Capturing _))) -> do
      tosm <- to (mo,po);
      enpa <- enPassantFieldPos po;
      Just $ MoveFromToOverwritingWithOtherDisappear (po,tosm) enpa
    (MkOutwardPawnMove (Capturing _,Nothing)) -> do
      tosm <- to (mo,po);
      enpa <- enPassantFieldPos po;
      Just $ MoveFromToOverwritingWithOtherDisappear (po,tosm) enpa

--TODO not all of these must be boolean
checkIfFigTypeOK :: StateMove -> Bool
checkIfFigTypeOK StateMove{move = (m,x), before = GameState {board=f}} =
  maybe False
  (((case m of
       (MkQueenMove _) -> Queen
       (MkKingMove _) -> King
       (MkBishopMove _) -> Bishop
       (MkRookMove _) -> Rook
       (MkInwardPawnMove _) -> InwardPawn
       (MkOutwardPawnMove _) -> OutwardPawn
       (MkKnightMove _) -> Knight
    )==).figType) (f x)
checkIfFigColorOK :: StateMove -> Bool
checkIfFigColorOK StateMove{move = (_,l), before = GameState {board=f, movesNext=c}} = maybe False ((c==).figColor) (f l)
whoMove :: StateMove -> Maybe Color
whoMove StateMove{move = (_,l), before = GameState {board=f}} = fmap figColor (f l)
whoHypoMove :: HypoStateMove -> Maybe Color
whoHypoMove HypoStateMove{hypoMove = (_,l), hypoBefore = (f,_,_)} = fmap figColor (f l)
checkIfIsEnPassant :: StateMove -> Bool
checkIfIsEnPassant sm = case fst $ move sm of
  MkOutwardPawnMove (Capturing _,Nothing) -> (do tosm <- to (move sm); Just $ isNothing $ board (before sm) tosm) == Just True
  _ -> False
whatColorThereIsPawnToEnPassant :: StateMove -> Maybe (Maybe Color)
whatColorThereIsPawnToEnPassant sm = case fst $ move sm of
  MkOutwardPawnMove (Capturing _,Nothing) -> do epsm <- enPassantFieldPosBM (move sm); Just $ figColor <$> board (before sm) epsm
  _ -> Nothing
checkIfSuchEnPassantPossible :: StateMove -> Bool
checkIfSuchEnPassantPossible sm = fromMaybe False $ checkIfSuchEnPassantPossibleMaybe sm
matchToColFun :: EnPassantMatch -> Color -> Color
matchToColFun LastMatch = prev
matchToColFun PrevMatch = next
checkIfSuchEnPassantPossibleMaybe :: StateMove -> Maybe Bool
checkIfSuchEnPassantPossibleMaybe sm = do
  whaCol <- whatColorThereIsPawnToEnPassant sm;
  who <- whoMove sm;
  tosm <- to (move sm);
  ma <- matchEnP (enPassantStore (before sm)) tosm
  Just $ whaCol == Just (matchToColFun ma who)
checkIfEnPassantImpossibility :: StateMove -> Bool
checkIfEnPassantImpossibility sm = checkIfIsEnPassant sm && not (checkIfSuchEnPassantPossible sm)
checkIfDestOpponent :: StateMove -> Bool
checkIfDestOpponent sm = fromMaybe False $ _checkIfCapturingSimplyMaybe sm
_checkIfCapturingSimplyMaybe :: StateMove -> Maybe Bool
_checkIfCapturingSimplyMaybe sm = do
  who <- whoMove sm;
  tosm <- to (move sm);
  oppo <- figColor <$> board (before sm) tosm;
  Just $ oppo /= who
checkIfDestEmpty :: StateMove -> Bool
checkIfDestEmpty sm = (Nothing==) $ board (before sm) <$> to (move sm)
wouldBeDestEmpty :: HypoStateMove -> Bool
wouldBeDestEmpty sm = (Nothing==) $ hypoBoard (hypoBefore sm) <$> hTo (hypoMove sm)
_wouldBeDestOpponentSimplyMaybe :: HypoStateMove -> Maybe Bool
_wouldBeDestOpponentSimplyMaybe sm = do
  who <- whoHypoMove sm;
  tosm <- hTo (hypoMove sm);
  oppo <- figColor <$> hypoBoard (hypoBefore sm) tosm;
  Just $ oppo /= who
wouldBeDestOpponent :: HypoStateMove -> Bool
wouldBeDestOpponent sm = fromMaybe False $ _wouldBeDestOpponentSimplyMaybe sm
data EmptyOrOccupiedUnlessEnPassant = Empty | OccupiedUnlessEnPassant deriving (Eq, Show)
mustDestBeEmptyOrOccupied :: MoveT -> Maybe EmptyOrOccupiedUnlessEnPassant
mustDestBeEmptyOrOccupied (MkInwardPawnMove (Walk Forward)) = Just Empty
mustDestBeEmptyOrOccupied (MkQueenMove _) = Nothing
mustDestBeEmptyOrOccupied (MkKingMove (NotAlone _)) = Just Empty
mustDestBeEmptyOrOccupied (MkKingMove _) = Nothing
mustDestBeEmptyOrOccupied (MkKnightMove _) = Nothing
mustDestBeEmptyOrOccupied (MkRookMove _) = Nothing
mustDestBeEmptyOrOccupied (MkBishopMove _) = Nothing
mustDestBeEmptyOrOccupied (MkInwardPawnMove (Walk (Capturing _))) = Just OccupiedUnlessEnPassant
mustDestBeEmptyOrOccupied (MkInwardPawnMove Jump) = Just Empty
mustDestBeEmptyOrOccupied (MkOutwardPawnMove (Capturing _,_)) = Just OccupiedUnlessEnPassant
mustDestBeEmptyOrOccupied (MkOutwardPawnMove (Forward,_)) = Just Empty
mustDestBeEmpty :: MoveT -> Bool
mustDestBeEmpty x = mustDestBeEmptyOrOccupied x == Just Empty
mustDestBeOccupiedUnlessEnPassant :: MoveT -> Bool
mustDestBeOccupiedUnlessEnPassant x = mustDestBeEmptyOrOccupied x == Just OccupiedUnlessEnPassant

checkIfCapturingOwnPiece :: StateMove -> Bool
checkIfCapturingOwnPiece sm = not $ checkIfDestEmpty sm || checkIfDestOpponent sm
emptiesMT :: BoundMoveT -> Maybe [Pos]
emptiesMT (m,f) = emptiesFromEBC f (vectorFromMoveT m)
emptiesHMT :: BoundHypoCapMoveT -> Maybe [Pos]
emptiesHMT (m,f) = emptiesFromEBC f (vectorFromHypoCapMoveT m)
checkIfAllAreEmptiesMaybe :: StateMove -> Maybe Bool
checkIfAllAreEmptiesMaybe sm = do
  empties <- emptiesMT $ move sm;
  Just $ checkEmpties (board (before sm)) empties
checkIfAllAreEmpties :: StateMove -> Bool
checkIfAllAreEmpties sm = fromMaybe False (checkIfAllAreEmptiesMaybe sm)
_wouldBeAllEmptiesMaybe :: HypoStateMove -> Maybe Bool
_wouldBeAllEmptiesMaybe sm = do
  empties <- emptiesHMT $ hypoMove sm;
  Just $ checkEmpties (hypoBoard (hypoBefore sm)) empties
wouldBeAllEmpties :: HypoStateMove -> Bool
wouldBeAllEmpties sm = fromMaybe False $ _wouldBeAllEmptiesMaybe sm
checkIfNoCastlingImpossibility :: StateMove -> Bool
checkIfNoCastlingImpossibility sm = fromMaybe True $ _checkCastlingImpossibilityMaybeHelper sm
_extractCastling :: MoveT -> Maybe Castling
_extractCastling (MkKingMove (NotAlone x)) = Just x
_extractCastling _ = Nothing
_checkCastlingImpossibilityMaybeHelper :: StateMove -> Maybe Bool
_checkCastlingImpossibilityMaybeHelper sm = do
  wh <- whoMove sm;
  cas <- _extractCastling $ fst $ move sm;
  Just $ castlingGetC (castlingPossibilities (before sm)) wh cas
isEmptyList :: [a] -> Bool
isEmptyList [] = True
isEmptyList _ = False
checkIfCapturingThruMoats :: StateMove -> Bool
checkIfCapturingThruMoats sm = checkIfDestOpponent sm && (not.isEmptyList) (moatsM (move sm))
wouldBeThruMoats :: BoundHypoCapMoveT -> Bool
wouldBeThruMoats sm = not.isEmptyList $ moatsHM sm
checkIfWeArePassingAnUnbridgedMoat :: StateMove -> Bool
checkIfWeArePassingAnUnbridgedMoat sm = not.isEmptyList $ filter (Unbridged==) $ fmap (isBridged $ moatsState $ before sm) $ moatsM $ move sm
checkIfThereIsNoCreekAgainstUs :: BoundMoveT -> Bool
checkIfThereIsNoCreekAgainstUs (MkInwardPawnMove (Walk (Capturing d)),(r,File _ se))
  | r<=MiddleOuter = not $ d==Pluswards && se==sevenSegmentEight || d==Minuswards && se==zeroSegmentEight
  | otherwise = True
checkIfThereIsNoCreekAgainstUs _ = True
hypoWouldBeNoCreak :: BoundHypoCapMoveT -> Bool
hypoWouldBeNoCreak (HypoInwardPawnMove d,(r,File _ se))
  | r<=MiddleOuter = not $ d==Pluswards && se==sevenSegmentEight || d==Minuswards && se==zeroSegmentEight
  | otherwise = True
hypoWouldBeNoCreak _ = True

data Impossibility where
  ThereIsACreakAgainstUs :: Impossibility
  WeArePassingAnUnbridgedMoat :: Impossibility
  WeAreCapturingThruMoats :: Impossibility
  ThereIsACastlingImpossibility :: Impossibility
  NotAllEmpties :: Impossibility
  WeAreCapturingOurOwnPiece :: Impossibility

checkImpossibility :: StateMove -> Maybe Impossibility
checkImpossibility sm
  | not $ checkIfThereIsNoCreekAgainstUs (move sm) = Just ThereIsACreakAgainstUs
  | checkIfWeArePassingAnUnbridgedMoat sm = Just WeArePassingAnUnbridgedMoat
  | checkIfCapturingThruMoats sm = Just WeAreCapturingThruMoats
  | not $ checkIfNoCastlingImpossibility sm = Just ThereIsACastlingImpossibility
  | not $ checkIfAllAreEmpties sm = Just NotAllEmpties
  | checkIfCapturingOwnPiece sm = Just WeAreCapturingOurOwnPiece
  | otherwise = Nothing

data HypoImpossibility = WouldBeCreak | WouldBeThruMoat | WouldNotAllEmpties | WouldBeSameColor | WouldGoToEmpty

checkHypoImpossibility :: HypoStateMove -> Maybe HypoImpossibility
checkHypoImpossibility sm
  | not $ hypoWouldBeNoCreak (hypoMove sm) = Just WouldBeCreak
  | wouldBeThruMoats (hypoMove sm) = Just WouldBeThruMoat
  | not $ wouldBeAllEmpties sm = Just WouldNotAllEmpties
  | wouldBeDestEmpty sm = Just WouldGoToEmpty
  | not $ wouldBeDestOpponent sm = Just WouldBeSameColor
  | otherwise = Nothing

data Cannot = Impossible Impossibility | WeMustPromote Bool | CheckInitiatedThruMoatException

-- _threatCheckingHelperOne :: GameBoard -> Pos -> PlayersAlive -> EnPassantStore
_isThereAThreatHelperOne :: GameBoard -> Pos -> Pos -> PlayersAlive -> EnPassantStore -> Maybe Bool
_isThereAThreatHelperOne this toP fromP pA ePS = do
  figt <- fmap figType (this fromP);
  Just $ let bef = hypoConstruct this (lastEnP ePS) pA in
           let vemovs = hypoMovesFromToWith figt fromP toP in
             --let vecs = vectorFromMoveT . disregardPromotionPossibOfEither . hypoMoveToNormalMove <$> vemovs in
             isEmptyList $ fmap fromJust $ filter isJust $ checkHypoImpossibility . (\x -> HypoStateMove{hypoBefore=bef,hypoMove=(x,fromP)}) <$> vemovs
isThereAThreat :: GameBoard -> Pos -> Pos -> PlayersAlive -> EnPassantStore -> Bool
isThereAThreat this toP fromP pA ePS = fromMaybe False $ _isThereAThreatHelperOne this toP fromP pA ePS

firstMaybe :: [a] -> Maybe a
firstMaybe (x:_) = Just x
firstMaybe [] = Nothing
checkThreatToColorKing :: StateMove -> Color -> Maybe Bool
checkThreatToColorKing sm col = do
  tosm <- to $ move sm;
  wheKing <- firstMaybe $ whereIsFig (Figure King col) (board (before sm))
  newb <- boardSimplyAfter sm
  Just $ isThereAThreat newb wheKing tosm (playersAlive (before sm)) (enPassantStore (before sm))
checkForCheckInitiatedThruMoat :: StateMove -> Maybe Bool
checkForCheckInitiatedThruMoat sm = ((not.isEmptyList $ moatsM $ move sm)&&) <$> do
  wCo <- whoMove sm;
  a <- checkThreatToColorKing sm (prev wCo);
  b <- checkThreatToColorKing sm (next wCo);
  Just $ a && b
nextPlayer :: PlayersAlive -> Color -> Color
nextPlayer pa c = if isAlive pa (next c) then next c else prev c
afterHalfMoveClock :: StateMove -> Maybe Count
afterHalfMoveClock _ = Just Once
afterPlayersAlive :: StateMove -> Maybe PlayersAlive
afterPlayersAlive sm = Just $ playersAlive $ before sm
incMaybeCount :: Maybe Count -> Count
incMaybeCount Nothing = Once
incMaybeCount (Just a) = OnceMore a
simpleAfter :: StateMove -> Maybe GameState
simpleAfter sm = do
  b <- boardSimplyAfter sm;
  cp <- afterCastling (castlingPossibilities (before sm)) (move sm) (movesNext (before sm));
  paa <- afterPlayersAlive sm;
  Just GameState { board=b, moatsState=afterMoatsState sm, movesNext=nextPlayer (playersAlive (before sm)) (movesNext (before sm))
                 , castlingPossibilities = cp, enPassantStore = afterEnPassantStore sm, fullMoveCounter = Just $ incMaybeCount (fullMoveCounter (before sm))
                 , halfMoveClock = afterHalfMoveClock sm, playersAlive = paa }
afterWOblahblah :: StateMove -> Maybe GameState
afterWOblahblah sm = do
  sa <- simpleAfter sm;
  Just sa

moatsM :: BoundMoveT -> [MoatLocalization]
moatsM (m, f) = moats f (vectorFromMoveT m)
moatsHM :: BoundHypoCapMoveT -> [MoatLocalization]
moatsHM (m, f) = moats f (vectorFromHypoCapMoveT m)

afterMoatsState :: StateMove -> MoatsState
afterMoatsState _ = error "Not implemented TODO"
enPassantFieldPosBM :: BoundMoveT -> Maybe Pos
enPassantFieldPosBM (MkInwardPawnMove Jump, f) = Just $ fromJust $ enPassantFieldPos f
enPassantFieldPosBM _ = Nothing
afterEnPassantStore :: StateMove -> EnPassantStore
afterEnPassantStore (StateMove m GameState{enPassantStore=x}) = mappEnP (enPassantFieldPosBM m) x

afterColorCastling :: CastlingPossibilitiesForColor -> BoundMoveT -> Color -> CastlingPossibilitiesForColor
afterColorCastling _ (MkKingMove _, _) _ = noCastlingForColor
afterColorCastling b (MkRookMove _, (MostOuter, File cof seg)) coi
  | coi==cof && seg==SegmentEight (SegmentQuarter FirstHalf FirstHalf) FirstHalf = casOff QueensideCastling b
  | coi==cof && seg==SegmentEight (SegmentQuarter SecondHalf SecondHalf) SecondHalf = casOff KingsideCastling b
  | otherwise = b
afterColorCastling b _ _ = b
afterCastling :: CastlingPossibilities -> BoundMoveT -> Color -> Maybe CastlingPossibilities
afterCastling b mb coi =
  do tomb <- to mb;
     Just $
       let caspo = castlingSetColor b coi $ afterColorCastling (castlingGetColor b coi) mb coi in
         if rank tomb /= MostOuter then caspo
         else case colorSegmFile (file tomb) of
                SegmentEight (SegmentQuarter SecondHalf SecondHalf) SecondHalf -> casOffC (segmColor (file tomb)) KingsideCastling caspo
                SegmentEight (SegmentQuarter FirstHalf FirstHalf) FirstHalf -> casOffC (segmColor (file tomb)) QueensideCastling caspo
                a | a==kfm -> castlingSetColor caspo (segmColor (file tomb)) noCastlingForColor
                _ -> caspo

-- afterFirstStageHelper :: StateMove -> Maybe GameState
-- afterFirstStageHelper (StateMove (MkQueenMove x, f) bef) = Nothing
