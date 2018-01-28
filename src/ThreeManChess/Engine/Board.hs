module ThreeManChess.Engine.Board where

import Data.Data
import ThreeManChess.Engine.Pos

type Board a = Pos -> Maybe a

put :: Board a -> Pos -> Maybe a -> Board a
put before x what y | x==y = what
put before _ what x = before x

swap :: Board a -> Pos -> Pos -> Board a
swap before to from x = case x of to -> before from
                                  from -> before to
empty :: Board a
empty x = Nothing
-- starting :: Board a
-- starting (Pos 1 f) = 
