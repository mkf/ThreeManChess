module ThreeManChess.Engine.EnPassantStore where

import ThreeManChess.Engine.Pos

-- type EnPassantStore = Maybe (Maybe Pos, Pos)
type EnPassantStore = (Maybe Pos, Maybe Pos)
-- (prev,last)

prevEnP :: EnPassantStore -> Maybe Pos
prevEnP = fst
lastEnP :: EnPassantStore -> Maybe Pos
lastEnP = snd

data EnPassantMatch = PrevMatch | LastMatch
matchEnP :: EnPassantStore -> Pos -> Maybe EnPassantMatch
matchEnP s p | prevEnP s == Just p = Just PrevMatch
             | lastEnP s == Just p = Just LastMatch
             | otherwise = Nothing

mappEnP :: Maybe Pos -> EnPassantStore -> EnPassantStore
mappEnP a (_, b) = (b, a)

nullEnP :: EnPassantStore -> EnPassantStore
--nullEnP (_, a) = (a, Nothing)
nullEnP = mappEnP Nothing

appEnP :: Pos -> EnPassantStore -> EnPassantStore
appEnP = mappEnP . Just
