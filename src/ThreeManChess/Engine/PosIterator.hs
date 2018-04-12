-- {-# LANGUAGE RecursiveDo #-}

module ThreeManChess.Engine.PosIterator where

import ThreeManChess.Engine.Pos

allFilesBetweenRIncl :: File -> File -> [File]
allFilesBetweenRIncl x y = minus y : (if minus y == x then [] else allFilesBetweenRIncl x (minus y))
allFilesFrom :: File -> [File]
allFilesFrom x =  allFilesBetweenRIncl x x
-- allFilesFrom x =
--   mdo
--     xs <- [(minus x)]
--     xs <- if ((minus (head xs))==x) then xs else ((minus (head xs)):xs)
--     x:xs
allRanks :: [Rank]
allRanks = ranks
allPos2D :: [[Pos]]
allPos2D = fmap (\r -> fmap
                       (\f -> (r,f))
                       (allFilesFrom $ fileFromInt 0)) allRanks
allPos :: [Pos]
allPos = concat allPos2D
