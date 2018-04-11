module ThreeManChess.Engine.CastlingPossibilities where

import ThreeManChess.Engine.Possibilities

type CastlingPossibilitiesForColor = (Bool,Bool)
type CastlingPossibilities = (CastlingPossibilitiesForColor,CastlingPossibilitiesForColor,CastlingPossibilitiesForColor)
