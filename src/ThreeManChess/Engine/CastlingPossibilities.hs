module ThreeManChess.Engine.CastlingPossibilities where

import ThreeManChess.Engine.Possibilities
import ThreeManChess.Engine.Color

type CastlingPossibilitiesForColor = (Bool,Bool)
type CastlingPossibilities = (CastlingPossibilitiesForColor,CastlingPossibilitiesForColor,CastlingPossibilitiesForColor)

allCastlingForColor :: CastlingPossibilitiesForColor
allCastlingForColor = (True, True)
noCastlingForColor :: CastlingPossibilitiesForColor
noCastlingForColor = (False, False)
castlingGet :: CastlingPossibilitiesForColor -> Castling -> Bool
castlingGet (a,_) KingsideCastling = a
castlingGet (_,a) QueensideCastling = a
castlingSet :: CastlingPossibilitiesForColor -> Castling -> Bool -> CastlingPossibilitiesForColor
castlingSet (_,a) KingsideCastling b = (b,a)
castlingSet (a,_) QueensideCastling b = (a,b)
casOff :: Castling -> CastlingPossibilitiesForColor -> CastlingPossibilitiesForColor
casOff c x = castlingSet x c False

allCastling :: CastlingPossibilities
allCastling = (allCastlingForColor, allCastlingForColor, allCastlingForColor)
noCastling :: CastlingPossibilities
noCastling = (noCastlingForColor, noCastlingForColor, noCastlingForColor)
castlingGetColor :: CastlingPossibilities -> Color -> CastlingPossibilitiesForColor
castlingGetColor (x,_,_) White = x
castlingGetColor (_,x,_) Gray = x
castlingGetColor (_,_,x) Black = x
castlingSetColor :: CastlingPossibilities -> Color -> CastlingPossibilitiesForColor -> CastlingPossibilities
castlingSetColor (_,y,z) White x = (x,y,z)
castlingSetColor (y,_,z) Gray x = (y,x,z)
castlingSetColor (y,z,_) Black x = (y,z,x)
casOffC :: Color -> Castling -> CastlingPossibilities -> CastlingPossibilities
casOffC co ca b = castlingSetColor b co $ casOff ca $ castlingGetColor b co
