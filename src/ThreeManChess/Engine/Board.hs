module ThreeManChess.Engine.Board where

import ThreeManChess.Engine.Pos

type Board a = Pos -> Maybe a

put :: Board a -> Pos -> Maybe a -> Board a
put _ x what y | x==y = what
put before _ _ x = before x

swap :: Board a -> Pos -> Pos -> Board a
swap before to from x | x==to = before from
                      | x==from = before to
                      | otherwise = before x
empty :: Board a
empty _ = Nothing
-- starting :: Board a
-- starting (Pos 1 f) =
