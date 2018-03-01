{-# Language DeriveDataTypeable #-}
module ThreeManChess.Engine.Color where

import Data.Data

data Color = White | Gray | Black deriving (Eq, Ord, Read, Show, Data, Typeable)
-- colorSet :: Set.Set Color
-- colorSet = Set.fromList White Gray Black
next :: Color -> Color
next White = Gray
next Gray = Black
next Black = White
prev :: Color -> Color
prev Black = Gray
prev Gray = White
prev White = Black
colors :: [Color]
colors = [White, Gray, Black]
colorSegm :: Int -> Color
colorSegm 0 = White
colorSegm 1 = Gray
colorSegm 2 = Black
colorSegm _ = undefined
intColorSegm :: Color -> Int
intColorSegm White = 0
intColorSegm Gray = 1
intColorSegm Black = 2
intColorMaybe :: Maybe Color -> Int
intColorMaybe Nothing = 0
intColorMaybe (Just White) = 1
intColorMaybe (Just Gray) = 2
intColorMaybe (Just Black) = 3
colorMaybe :: Int -> Maybe Color
colorMaybe 0 = Nothing
colorMaybe 1 = Just White
colorMaybe 2 = Just Gray
colorMaybe 3 = Just Black
colorMaybe _ = undefined
