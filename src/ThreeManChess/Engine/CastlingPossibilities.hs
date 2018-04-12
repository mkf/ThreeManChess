module ThreeManChess.Engine.CastlingPossibilities where

import ThreeManChess.Engine.Possibilities

type CastlingPossibilitiesForColor = (Bool,Bool)
type CastlingPossibilities = (CastlingPossibilitiesForColor,CastlingPossibilitiesForColor,CastlingPossibilitiesForColor)

allCastlingForColor :: CastlingPossibilitiesForColor
allCastlingForColor = (True, True)

allCastling :: CastlingPossibilities
allCastling = (allCastlingForColor, allCastlingForColor, allCastlingForColor)
