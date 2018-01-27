module ThreeManChess.Engine.Pos where

-- import Data.Data
import ThreeManChess.Engine.Color

-- data Rank = 0 | 1 | 2 | 3 | 4 | 5
-- data File = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23
data Rank = MostOuter | SecondOuter | MiddleOuter | MiddleInner | SecondInner | MostInner deriving (Eq, Ord, Read, Show)
type ColorSegment = Color
data SegmentHalf = FirstHalf | SecondHalf deriving (Eq, Ord, Read, Show)
data SegmentQuarter = SegmentQuarter { half :: SegmentHalf , halfQuarter :: SegmentHalf }
  deriving (Eq, Ord, Read, Show)
data SegmentFile = SegmentFile { segmentQuarter :: SegmentQuarter , quarterFile :: SegmentHalf }
  deriving (Eq, Ord, Read, Show)
data File = File { color :: ColorSegment , segmentFile :: SegmentFile }
  deriving (Eq, Ord, Read, Show)

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
fileFromInt x = if x>=0 && x<24 then File {color = colorSegm $ div x 8,
                                           segmentFile = colorSegmFileFromInt $ mod x 8} else undefined
intFile :: File -> Int
intFile (File c s) = 8 * intColorSegm c + intColorSegmFile s
colorSegmFileFromInt :: Int -> SegmentFile
colorSegmFileFromInt x = if x>=0 && x<8 then SegmentFile { segmentQuarter = segmQuarterFromInt $ div x 2,
                                                           quarterFile = segmHalfFromInt $ mod x 2 } else undefined
intColorSegmFile :: SegmentFile -> Int
intColorSegmFile (SegmentFile q f) = 2 * intSegmQuarter q + intSegmHalf f
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

data Pos = Pos { rank :: Rank, file :: File }
