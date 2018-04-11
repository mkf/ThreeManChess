module ThreeManChess.Engine.GameState where

import ThreeManChess.Engine.GameBoard
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.CastlingPossibilities
import ThreeManChess.Engine.Possibilities
import ThreeManChess.Engine.Moats
import ThreeManChess.Engine.EnPassantStore
import ThreeManChess.Engine.PlayersAlive

data GameState = GameState {board :: GameBoard, moatsState :: MoatsState, movesNext :: Color, castlingPossibilities :: CastlingPossibilities,
                            enPassantStore :: EnPassantStore, halfMoveClock :: Maybe Count, fullMoveCounter :: Maybe Count, playersAlive :: PlayersAlive}
