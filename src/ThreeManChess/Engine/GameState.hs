{-# LANGUAGE DuplicateRecordFields #-}

module ThreeManChess.Engine.GameState where

import ThreeManChess.Engine.Board
import ThreeManChess.Engine.GameBoard
import ThreeManChess.Engine.Color
import ThreeManChess.Engine.CastlingPossibilities
import ThreeManChess.Engine.Moats
import ThreeManChess.Engine.EnPassantStore
import ThreeManChess.Engine.PlayersAlive
import ThreeManChess.Engine.Directions
import ThreeManChess.Engine.Pos

showBoard :: (Show a) => Board a -> String
showBoard a = show $ BoardWrap a

{-@ data GameState = GameState {board :: GameBoard, moatsState :: MoatsState, movesNext :: Color, castlingPossibilities :: CastlingPossibilities,
                            enPassantStore :: EnPassantStore, halfMoveClock :: NNegInt, fullMoveCounter :: NNegInt, playersAlive :: PlayersAlive } @-}
data GameState = GameState {board :: GameBoard, moatsState :: MoatsState, movesNext :: Color, castlingPossibilities :: CastlingPossibilities,
                            enPassantStore :: EnPassantStore, halfMoveClock :: Int, fullMoveCounter :: Int, playersAlive :: PlayersAlive }
instance Show GameState where
  show GameState{board=b, moatsState=ms, movesNext=mn, castlingPossibilities=cp, enPassantStore=eps, halfMoveClock=hmc, fullMoveCounter=fmc, playersAlive=pa} =
    showBoard b ++ show ms ++ show mn ++ show cp ++ show eps ++ show hmc ++ show fmc ++ show pa

newGame :: GameState
newGame = GameState { board = startBoard, moatsState = noBridges, movesNext = White, castlingPossibilities = allCastling,
                      enPassantStore = (Nothing, Nothing), halfMoveClock = 0, fullMoveCounter = 0, playersAlive = allAlive }

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
