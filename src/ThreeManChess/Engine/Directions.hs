module ThreeManChess.Engine.Directions where

import ThreeManChess.Engine.Pos

class --(Eq a , Read a, Show a) =>
  Reversable a where
  rever :: a -> a
data Orientation = Rankwise | Filewise deriving (Eq, Read, Show)
perpendicularTo :: Orientation -> Orientation
perpendicularTo Rankwise = Filewise
perpendicularTo Filewise = Rankwise
data RankwiseDirection = Inwards | Outwards deriving (Eq, Show)-- deriving StraightDirection
instance Reversable RankwiseDirection where
  rever Inwards = Outwards
  rever Outwards = Inwards
data FilewiseDirection = Pluswards | Minuswards deriving (Eq, Show)-- deriving StraightDirection
instance Reversable FilewiseDirection where
  rever Pluswards = Minuswards
  rever Minuswards = Pluswards
{-@ type PosInt = {v:Int|v>0} @-}
{-@ type NNegInt = {v:Int|v>=0} @-}
{-@ maybePosInt :: NNegInt -> Maybe PosInt @-}
maybePosInt :: Int -> Maybe Int
maybePosInt n
  | n==0 = Nothing
  | n>0 = Just n
{-@ pmMod24Oper :: FilewiseDirection -> NNegInt -> File -> File @-}
pmMod24Oper :: FilewiseDirection -> Int -> File -> File
pmMod24Oper Pluswards n f = mod (f + n) 24
pmMod24Oper Minuswards n f = mod (f + 24 - n) 24
filewiseInc :: FilewiseDirection -> File -> File
filewiseInc Pluswards = plus
filewiseInc Minuswards = minus
data DiagonalDirection = DiagonalDirection RankwiseDirection FilewiseDirection deriving (Eq, Show) -- deriving LinearDirection
rankwise :: DiagonalDirection -> RankwiseDirection
rankwise (DiagonalDirection x _) = x
filewise :: DiagonalDirection -> FilewiseDirection
filewise (DiagonalDirection _ x) = x
data StraightDirecEBC = MkRankwiseDirecEBC RankwiseDirection | MkFilewiseDirecEBC FilewiseDirection deriving (Show, Eq)
data LinearDirecEBC = MkStraightDirecEBC StraightDirecEBC | MkDiagonalDirecEBC DiagonalDirection deriving (Show, Eq)
instance Reversable DiagonalDirection where
  rever (DiagonalDirection a b) = DiagonalDirection (rever a) (rever b)

data PlusMinus a = Plus a | Minus a deriving (Eq, Read, Show)
absPlusMinus :: PlusMinus a -> a
absPlusMinus (Plus a) = a
absPlusMinus (Minus a) = a