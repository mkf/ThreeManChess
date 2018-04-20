{-# LANGUAGE DuplicateRecordFields #-}

module ThreeManChess.Engine.GameState where

import ThreeManChess.Engine.GameBoard
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.CastlingPossibilities
import ThreeManChess.Engine.Moats
import ThreeManChess.Engine.EnPassantStore
import ThreeManChess.Engine.PlayersAlive
import ThreeManChess.Engine.Directions
import ThreeManChess.Engine.Pos

data GameState = GameState {board :: GameBoard, moatsState :: MoatsState, movesNext :: Color, castlingPossibilities :: CastlingPossibilities,
                            enPassantStore :: EnPassantStore, halfMoveClock :: Maybe Count, fullMoveCounter :: Maybe Count, playersAlive :: PlayersAlive }
newGame :: GameState
newGame = GameState { board = startBoard, moatsState = noBridges, movesNext = White, castlingPossibilities = allCastling,
                      enPassantStore = (Nothing, Nothing), halfMoveClock = Nothing, fullMoveCounter = Nothing, playersAlive = allAlive }

data HypoCheckState = HypoCheckState {board :: GameBoard, enPassantStore :: Maybe Pos, playersAlive :: PlayersAlive}

gameToHypo :: GameState -> HypoCheckState
gameToHypo GameState{board=b, enPassantStore=e, playersAlive=a} = HypoCheckState{board=b, enPassantStore= lastEnP e, playersAlive=a}
