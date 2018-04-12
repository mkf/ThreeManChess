module ThreeManChess.Engine.Pos where

-- import Data.Data
import ThreeManChess.Engine.Color

-- data Rank = 0 | 1 | 2 | 3 | 4 | 5
-- data File = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23
data Rank = MostOuter | SecondOuter | MiddleOuter | MiddleInner | SecondInner | MostInner deriving (Eq, Ord, Read, Show)
ranks :: [Rank]
ranks = [MostOuter, SecondOuter, MiddleOuter, MiddleInner, SecondInner, MostInner]
type ColorSegment = Color
data SegmentHalf = FirstHalf | SecondHalf deriving (Eq, Ord, Read, Show)
data SegmentQuarter = SegmentQuarter { half :: SegmentHalf , halfQuarter :: SegmentHalf }
  deriving (Ord, Read, Show)
instance Eq SegmentQuarter where
  x == y = half x == half y && halfQuarter x == halfQuarter y
data SegmentEight = SegmentEight { segmentQuarter :: SegmentQuarter , quarterHalf :: SegmentHalf }
  deriving (Ord, Read, Show)
instance Eq SegmentEight where
  x == y = segmentQuarter x == segmentQuarter y && quarterHalf x == quarterHalf y
data File = File { segmColor :: ColorSegment , colorSegmFile :: SegmentEight }
  deriving (Ord, Read, Show)
instance Eq File where
  x == y = segmColor x == segmColor y && colorSegmFile x == colorSegmFile y
opposite :: File -> File
opposite (File c (SegmentEight (SegmentQuarter h q) f)) =
  File {segmColor= case h of FirstHalf -> next
                             SecondHalf -> prev
               $ c,
        colorSegmFile=SegmentEight {
           segmentQuarter = SegmentQuarter { half=otherSegmHalf h, halfQuarter=q },
           quarterHalf = f }}
minus :: File -> File
minus File { segmColor=c, colorSegmFile= SegmentEight {segmentQuarter= (SegmentQuarter FirstHalf FirstHalf),
                                                 quarterHalf = FirstHalf}} =
  File {segmColor=prev c,
        colorSegmFile = SegmentEight {segmentQuarter=SegmentQuarter SecondHalf SecondHalf,
                                     quarterHalf = SecondHalf}}
minus File { segmColor=c, colorSegmFile=f} =
  case f of
    SegmentEight {segmentQuarter= (SegmentQuarter FirstHalf FirstHalf),
                  quarterHalf = FirstHalf} ->
      File {segmColor=prev c,
            colorSegmFile = SegmentEight {segmentQuarter=SegmentQuarter SecondHalf SecondHalf,
                                         quarterHalf = SecondHalf}}
    _ ->
      File {segmColor=c, colorSegmFile=
                     case f of
                       SegmentEight{segmentQuarter=q,quarterHalf=SecondHalf} ->
                         SegmentEight{segmentQuarter=q, quarterHalf=FirstHalf}
                       SegmentEight{segmentQuarter=SegmentQuarter h SecondHalf} ->
                         SegmentEight{segmentQuarter=SegmentQuarter h FirstHalf, quarterHalf=SecondHalf}
                       SegmentEight{segmentQuarter=SegmentQuarter SecondHalf FirstHalf} ->
                         SegmentEight{segmentQuarter=SegmentQuarter FirstHalf SecondHalf, quarterHalf=SecondHalf}
                       _ -> error "case did not work"
}
plus :: File -> File
plus File { segmColor=c, colorSegmFile=f} =
  case f of
    SegmentEight {segmentQuarter= (SegmentQuarter SecondHalf SecondHalf),
                  quarterHalf = SecondHalf} ->
      File {segmColor=next c,
            colorSegmFile = SegmentEight {segmentQuarter=SegmentQuarter FirstHalf FirstHalf,
                                         quarterHalf = FirstHalf}}
    _ ->
      File {segmColor=c, colorSegmFile=
                     case f of
                       SegmentEight{segmentQuarter=q,quarterHalf=FirstHalf} ->
                         SegmentEight{segmentQuarter=q, quarterHalf=SecondHalf}
                       SegmentEight{segmentQuarter=SegmentQuarter h FirstHalf} ->
                         SegmentEight{segmentQuarter=SegmentQuarter h SecondHalf, quarterHalf=SecondHalf}
                       SegmentEight{segmentQuarter=SegmentQuarter FirstHalf SecondHalf} ->
                         SegmentEight{segmentQuarter=SegmentQuarter SecondHalf FirstHalf, quarterHalf=FirstHalf}
                       _ -> error "case did not work"
}
inw :: Rank -> Rank
inw MostOuter = SecondOuter
inw SecondOuter = MiddleOuter
inw MiddleOuter = MiddleInner
inw MiddleInner = SecondInner
inw SecondInner = MostInner
inw MostInner = MostInner
out :: Rank -> Maybe Rank
out MostInner = Just SecondInner
out SecondInner = Just MiddleInner
out MiddleInner = Just MiddleOuter
out MiddleOuter = Just SecondOuter
out SecondOuter = Just MostOuter
out MostOuter = Nothing
rankFromInt :: Int -> Rank
rankFromInt 0 = MostOuter
rankFromInt 1 = SecondOuter
rankFromInt 2 = MiddleOuter
rankFromInt 3 = MiddleInner
rankFromInt 4 = SecondInner
rankFromInt 5 = MostInner
rankFromInt _ = undefined
intRank :: Rank -> Int
intRank MostOuter = 0
intRank SecondOuter = 1
intRank MiddleOuter = 2
intRank MiddleInner = 3
intRank SecondInner = 4
intRank MostInner = 5
fileFromInt :: Int -> File
fileFromInt x = if x>=0 && x<24 then File {segmColor = colorSegm $ div x 8,
                                           colorSegmFile = segmEightFromInt $ mod x 8} else undefined
intFile :: File -> Int
intFile (File c s) = 8 * intColorSegm c + intColorSegmEight s
segmEightFromInt :: Int -> SegmentEight
segmEightFromInt x = if x>=0 && x<8 then SegmentEight { segmentQuarter = segmQuarterFromInt $ div x 2,
                                                             quarterHalf = segmHalfFromInt $ mod x 2 } else undefined
intColorSegmEight :: SegmentEight -> Int
intColorSegmEight (SegmentEight q f) = 2 * intSegmQuarter q + intSegmHalf f
segmQuarterFromInt :: Int -> SegmentQuarter
segmQuarterFromInt x = if x>=0 && x<4 then SegmentQuarter { half = segmHalfFromInt $ div x 2,
                                                            halfQuarter = segmHalfFromInt $ mod x 2 } else undefined
intSegmQuarter :: SegmentQuarter -> Int
intSegmQuarter (SegmentQuarter h q) = 2 * intSegmHalf h + intSegmHalf q
segmHalfFromInt :: Int -> SegmentHalf
segmHalfFromInt 0 = FirstHalf
segmHalfFromInt 1 = SecondHalf
segmHalfFromInt _ = undefined
intSegmHalf :: SegmentHalf -> Int
intSegmHalf FirstHalf = 0
intSegmHalf SecondHalf = 1
otherSegmHalf :: SegmentHalf -> SegmentHalf
otherSegmHalf FirstHalf = SecondHalf
otherSegmHalf SecondHalf = FirstHalf

-- data Pos = Pos Rank File deriving (Eq, Show, Read)
type Pos = (Rank, File)
rank :: Pos -> Rank
rank (r, _) = r
file :: Pos -> File
file (_, f) = f
