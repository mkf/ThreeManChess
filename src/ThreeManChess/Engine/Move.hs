module ThreeManChess.Engine.Move where

import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.Possibilities

data (Vec a) => BoundVec a = BoundVec a Pos
