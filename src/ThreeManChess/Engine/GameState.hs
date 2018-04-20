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

-- data HypoCheckState = HypoCheckState {board :: GameBoard, enPassantStoreLast :: Maybe Pos, playersAlive :: PlayersAlive}
type HypoCheckState = (GameBoard, Maybe Pos, PlayersAlive)

gameToHypo :: GameState -> HypoCheckState
-- gameToHypo GameState{board=b, enPassantStore=e, playersAlive=a} = HypoCheckState{board=b, enPassantStoreLast=lastEnP e, playersAlive=a}
gameToHypo GameState{board=b, enPassantStore=e, playersAlive=a} = (b,lastEnP e, a)

hypoBoard :: HypoCheckState -> GameBoard
hypoBoard (x,_,_) = x
hypoEnPassantLast :: HypoCheckState -> Maybe Pos
hypoEnPassantLast (_,x,_) = x
hypoPlayersAlive :: HypoCheckState -> PlayersAlive
hypoPlayersAlive (_,_,x) = x
hypoConstruct :: GameBoard -> Maybe Pos -> PlayersAlive -> HypoCheckState
hypoConstruct a b c = (a,b,c)


-- class GState a where
--   board :: a -> GameBoard
--   playersAlive :: a -> PlayersAlive

-- instance GState GameState
-- instance GState HypoCheckState
