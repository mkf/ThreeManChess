{-# LANGUAGE DeriveDataTypeable #-}

module ThreeManChess.Engine.Directions where

import ThreeManChess.Engine.Pos
import Data.Data

class --(Eq a , Read a, Show a) =>
  Reversable a where
  rever :: a -> a
data Orientation = Rankwise | Filewise deriving (Eq, Read, Show)
perpendicularTo :: Orientation -> Orientation
perpendicularTo Rankwise = Filewise
perpendicularTo Filewise = Rankwise
data RankwiseDirection = Inwards | Outwards deriving (Eq, Data, Typeable, Show)-- deriving StraightDirection
instance Reversable RankwiseDirection where
  rever Inwards = Outwards
  rever Outwards = Inwards
data FilewiseDirection = Pluswards | Minuswards deriving (Data, Typeable, Show)-- deriving StraightDirection
instance Reversable FilewiseDirection where
  rever Pluswards = Minuswards
  rever Minuswards = Pluswards
filewiseInc :: FilewiseDirection -> File -> File
filewiseInc Pluswards = plus
filewiseInc Minuswards = minus
instance Eq FilewiseDirection where
  Pluswards == Pluswards = True
  Minuswards == Minuswards = True
  Pluswards == Minuswards = False
  Minuswards == Pluswards = False
data DiagonalDirection = DiagonalDirection RankwiseDirection FilewiseDirection deriving Show -- deriving LinearDirection
rankwise :: DiagonalDirection -> RankwiseDirection
rankwise (DiagonalDirection x _) = x
filewise :: DiagonalDirection -> FilewiseDirection
filewise (DiagonalDirection _ x) = x
data StraightDirecEBC = MkRankwiseDirecEBC RankwiseDirection | MkFilewiseDirecEBC FilewiseDirection deriving (Show, Eq)
data LinearDirecEBC = MkStraightDirecEBC StraightDirecEBC | MkDiagonalDirecEBC DiagonalDirection deriving (Show, Eq)
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
absSubstractCount = curry $ fmap absPlusMinus . uncurry substractCount
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
