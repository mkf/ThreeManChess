{-# LANGUAGE TypeFamilies, DataKinds, ExistentialQuantification, GADTs #-}

module ThreeManChess.Engine.Move where

import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.Figure
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.Possibilities
import ThreeManChess.Engine.FigType
import ThreeManChess.Engine.GameState
import ThreeManChess.Engine.GameBoard
import ThreeManChess.Engine.Board
import ThreeManChess.Engine.Moats
import ThreeManChess.Engine.CastlingPossibilities
import ThreeManChess.Engine.EnPassantStore
import Data.Maybe

data (Vec a) => BoundVec a = BoundVec a Pos

data KingMove = AloneDiagonally DiagonalDirection | AloneRankwise RankwiseDirection | AloneFilewise FilewiseDirection | NotAlone Castling
data JustPawnMove = Forward | Capturing FilewiseDirection
data PossiblyJumpingPawnMove = Walk JustPawnMove | Jump
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
vectorFromMoveT (MkKingMove (AloneDiagonally x)) = MkLinearVecEBC $ MkDiagonalVecEBC $ LinearVec x Once
vectorFromMoveT (MkKingMove (AloneRankwise x)) = MkLinearVecEBC $ MkStraightVecEBC $ MkRankwiseVecEBC $ LinearVec x Once
vectorFromMoveT (MkKingMove (AloneFilewise x)) = MkLinearVecEBC $ MkStraightVecEBC $ MkFilewiseVecEBC $ LinearVec x Once
vectorFromMoveT (MkKingMove (NotAlone x)) = MkCastlingVecEBC x
vectorFromMoveT (MkRookMove x) = MkLinearVecEBC $ MkStraightVecEBC x
vectorFromMoveT (MkBishopMove x) = MkLinearVecEBC $ MkDiagonalVecEBC x
vectorFromMoveT (MkKnightMove x) = MkKnightVecEBC x
vectorFromMoveT (MkInwardPawnMove (Walk Forward)) = MkLinearVecEBC $ MkStraightVecEBC $ MkRankwiseVecEBC $ LinearVec Inwards Once
vectorFromMoveT (MkInwardPawnMove (Walk (Capturing x))) = MkLinearVecEBC $ MkDiagonalVecEBC $ LinearVec (DiagonalDirection Inwards x) Once
vectorFromMoveT (MkInwardPawnMove Jump) = MkPawnJumpByTwoVecEBC PawnJumpByTwo
vectorFromMoveT (MkOutwardPawnMove (Forward,_)) = MkLinearVecEBC $ MkStraightVecEBC $ MkRankwiseVecEBC $ LinearVec Outwards Once
vectorFromMoveT (MkOutwardPawnMove (Capturing x,_)) = MkLinearVecEBC $ MkDiagonalVecEBC $ LinearVec (DiagonalDirection Outwards x) Once

vecsFromToWith :: FigType -> Pos -> Pos -> Color -> [VecC]
vecsFromToWith Queen a b _ = fmap (\(MkLinearVecC x) -> (MkVecC x)) (fromToLinear a b)
vecsFromToWith King a b col
  | maybe False (b==) (addOne (DiagonalDirection Inwards Pluswards) a) = [MkVecC $ flip LinearVec Once $ DiagonalDirection Inwards Pluswards]
  | maybe False (b==) (addOne (DiagonalDirection Inwards Minuswards) a) = [MkVecC $ flip LinearVec Once $ DiagonalDirection Inwards Minuswards]
  | maybe False (b==) (addOne (DiagonalDirection Outwards Pluswards) a) = [MkVecC $ flip LinearVec Once $ DiagonalDirection Outwards Pluswards]
  | maybe False (b==) (addOne (DiagonalDirection Outwards Minuswards) a) = [MkVecC $ flip LinearVec Once $ DiagonalDirection Outwards Minuswards]
  | maybe False (b==) (addOne Pluswards a) = [MkVecC $ LinearVec Pluswards Once]
  | maybe False (b==) (addOne Minuswards a) = [MkVecC $ LinearVec Minuswards Once]
  | maybe False (b==) (addOne Inwards a) = [MkVecC $ LinearVec Inwards Once]
  | maybe False (b==) (addOne Outwards a) = [MkVecC $ LinearVec Outwards Once]
  | a==(MostOuter, File col kfm) && maybe False (b==) (add a QueensideCastling) = [MkVecC QueensideCastling]
  | a==(MostOuter, File col kfm) && maybe False (b==) (add a KingsideCastling) = [MkVecC KingsideCastling]
  | otherwise = []
vecsFromToWith Rook a b _ = fmap (\(MkStraightVecC x) -> (MkVecC x)) (fromToStraight a b)
vecsFromToWith Bishop a b _ = MkVecC <$> fromToDiagonal a b
vecsFromToWith Knight a b _ = MkVecC <$> fromToKnight a b
vecsFromToWith InwardPawn a b col
  | maybe False (b==) (addOne Inwards a) = [MkVecC $ LinearVec Inwards Once]
  | maybe False (b==) (addOne (DiagonalDirection Inwards Pluswards) a) = [MkVecC $ LinearVec (DiagonalDirection Inwards Pluswards) Once]
  | maybe False (b==) (addOne (DiagonalDirection Inwards Minuswards) a) = [MkVecC $ LinearVec (DiagonalDirection Inwards Minuswards) Once]
  | rank a ==SecondOuter && segmColor (file a) == col = [MkVecC PawnJumpByTwo]
  | otherwise = []
vecsFromToWith OutwardPawn a b _
  | maybe False (b==) (addOne Outwards a) = [MkVecC $ LinearVec Outwards Once]
  | maybe False (b==) (addOne (DiagonalDirection Outwards Pluswards) a) = [MkVecC $ LinearVec (DiagonalDirection Outwards Pluswards) Once]
  | maybe False (b==) (addOne (DiagonalDirection Outwards Minuswards) a) = [MkVecC $ LinearVec (DiagonalDirection Outwards Minuswards) Once]
  | otherwise = []

-- moveFromVecWith :: FigType -> VecC -> Either (Maybe Promotion -> MoveT) MoveT
-- moveFromVecWith Queen (MkVecC (LinearVec a b)) = Right $ MkQueenMove (MkLinearVecC a b)
moveFromVecWith :: FigType -> VecEBC -> Maybe (Either (Maybe Promotion -> MoveT) MoveT)
moveFromVecWith Queen (MkLinearVecEBC x) = Just $ Right $ MkQueenMove x
moveFromVecWith Queen _ = Nothing
moveFromVecWith King (MkLinearVecEBC (MkDiagonalVecEBC (LinearVec a Once))) = Just $ Right $ MkKingMove (AloneDiagonally a)
moveFromVecWith King (MkLinearVecEBC (MkStraightVecEBC (MkRankwiseVecEBC (LinearVec a Once)))) = Just $ Right $ MkKingMove (AloneRankwise a)
moveFromVecWith King (MkLinearVecEBC (MkStraightVecEBC (MkFilewiseVecEBC (LinearVec a Once)))) = Just $ Right $ MkKingMove (AloneFilewise a)
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

data MoveT where
  MkQueenMove :: Move 'Queen -> MoveT
  MkKingMove :: Move 'King -> MoveT
  MkRookMove :: Move 'Rook -> MoveT
  MkBishopMove :: Move 'Bishop -> MoveT
  MkKnightMove :: Move 'Knight -> MoveT
  MkInwardPawnMove :: Move 'InwardPawn -> MoveT
  MkOutwardPawnMove :: Move 'OutwardPawn -> MoveT

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
checkIfPromotionPresenceIsOKforOP :: BoundMove 'OutwardPawn -> Bool
checkIfPromotionPresenceIsOKforOP ((_,Just _),(SecondOuter,_)) = True
checkIfPromotionPresenceIsOKforOP ((_,Nothing),_) = True
checkIfPromotionPresenceIsOKforOP _ = False
checkIfPromotionPresenceIsOK :: BoundMoveT -> Bool
checkIfPromotionPresenceIsOK (MkOutwardPawnMove x,f) = checkIfPromotionPresenceIsOKforOP (x,f)
checkIfPromotionPresenceIsOK _ = True

-- boardSimplyAfter :: GameBoard -> BoundMoveT

data StateMove = StateMove {move :: BoundMoveT, before :: GameState}

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
checkIfIsEnPassant :: StateMove -> Bool
checkIfIsEnPassant _ = error "Not implemented TODO"
checkIfEnPassantPossible :: StateMove -> Bool
checkIfEnPassantPossible _ = error "Not implemented TODO"
checkIfCapturing :: StateMove -> Bool
checkIfCapturing _ = error "Not implemented TODO"
checkIfCapturingOwnPiece :: StateMove -> Bool
checkIfCapturingOwnPiece _ = error "Not implemented TODO"
checkIfAllAreEmpties :: StateMove -> Bool
checkIfAllAreEmpties _ = error "Not implemented TODO"
checkCastlingImpossibility :: StateMove -> Bool
checkCastlingImpossibility _ = error "Not implemented TODO"
checkIfCapturingThruMoats :: StateMove -> Bool
checkIfCapturingThruMoats _ = error "Not implemented TODO"
checkIfWeArePassingAnUnbridgedMoat :: StateMove -> Bool
checkIfWeArePassingAnUnbridgedMoat _ = error "Not implemented TODO"

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
