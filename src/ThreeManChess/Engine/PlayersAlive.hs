module ThreeManChess.Engine.PlayersAlive where

import ThreeManChess.Engine.Color

type PlayersAlive = (Bool,Bool,Bool)

allAlive :: PlayersAlive
allAlive = (True, True, True)

isAlive :: PlayersAlive -> Color -> Bool
isAlive (x,_,_) White = x
isAlive (_,x,_) Gray = x
isAlive (_,_,x) Black = x
