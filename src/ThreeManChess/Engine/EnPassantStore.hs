module ThreeManChess.Engine.EnPassantStore where

import ThreeManChess.Engine.Pos

type EnPassantStore = Maybe (Maybe Pos, Pos)
