{-# Language DataKinds, KindSignatures #-}
module ThreeManChess.Engine.FigType where

-- import Data.Set as Set

data FigType = InwardPawn | OutwardPawn | Rook | Knight | Bishop | King | Queen deriving (Eq, Read)
instance Show FigType where
  show InwardPawn = "↑"
  show OutwardPawn = "↓"
  show Rook = "R"
  show Knight = "N"
  show Bishop = "B"
  show King = "K"
  show Queen = "Q"

class FigTypeClass f
instance FigTypeClass FigType

-- figTypeSet :: Set.Set FigType
-- figTypeSet = Set.fromList [Pawn, Rook, Knight, Bishop, King, Queen]
figTypes :: [FigType]
figTypes = [InwardPawn,OutwardPawn,Rook,Knight,Bishop,King,Queen]

data Promotion = RookPromotion | KnightPromotion | BishopPromotion | QueenPromotion deriving (Eq, Show)
desiredType :: Promotion -> FigType
desiredType RookPromotion = Rook
desiredType KnightPromotion = Knight
desiredType BishopPromotion = Bishop
desiredType QueenPromotion = Queen

class PromotionDesire (a :: FigType)

instance PromotionDesire 'Rook
instance PromotionDesire 'Knight
instance PromotionDesire 'Bishop
instance PromotionDesire 'Queen
