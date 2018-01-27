module ThreeManChess.Engine.Color where

data Color = White | Gray | Black deriving (Eq, Ord, Read, Show)
-- colorSet :: Set.Set Color
-- colorSet = Set.fromList White Gray Black
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
