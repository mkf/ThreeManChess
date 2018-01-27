module ThreeManChess.Engine.Board where

import Data.Data
import ThreeManChess.Engine.Pos

type Board a = Pos -> Maybe a

put :: Board a -> Pos -> Maybe a -> Board a
put before where_to what x = if x==where_to then what else before x

swap :: Board a -> Pos -> Pos -> Board a
swap before to from x = case x of to -> before from
                                  from -> before to
empty :: Board a
empty x = Nothing
-- starting :: Board a
-- starting (Pos 1 f) = 
