module ThreeManChess.Engine.Pos where

-- import Data.Data
import ThreeManChess.Engine.Color

{-@ type Int0toNexcl N = {v:Int | (v>=0) && (v<N) } @-}
{-@ type Rank = Int0toNexcl 6 @-}
type Rank = Int
{-@ type File = Int0toNexcl 24 @-}
type File = Int
ranks :: [Rank]
ranks = [0,1,2,3,4,5]
type ColorSegment = Color
{-@ type SegmentHalf = Int0toNexcl 2 @-}
type SegmentHalf = Int
{-@ type SegmentQuarter = Int0toNexcl 4 @-}
type SegmentQuarter = Int
{-@ type SegmentEight = Int0toNexcl 8 @-}
type SegmentEight = Int
{-@ segmColor :: File -> ColorSegment @-}
segmColor :: File -> ColorSegment
segmColor f = colorSegm $ quot f 8
{-@ segmFile :: File -> SegmentEight @-}
segmFile :: File -> SegmentEight
segmFile f = mod f 8
{-@ colorSegmFile :: File -> SegmentEight @-}
colorSegmFile :: File -> SegmentEight
colorSegmFile = segmFile
{-@ fileToColorAndSegmFile :: File -> (ColorSegment, SegmentEight) @-}
fileToColorAndSegmFile :: File -> (ColorSegment, SegmentEight)
fileToColorAndSegmFile f = (segmColor f, segmFile f)
fileFromColorAndSegmFile :: (ColorSegment, SegmentEight) -> File
fileFromColorAndSegmFile (c,e) = intColorSegm c * 8 + e
opposite :: File -> File
opposite f
  | f<12 = f+12
  | otherwise = f-12
minus :: File -> File
minus 0 = 23
minus f = f-1
plus :: File -> File
plus 23 = 0
plus f = f+1
{-@ mirrorHalf :: SegmentHalf -> SegmentHalf @-}
mirrorHalf :: SegmentHalf -> SegmentHalf
mirrorHalf 0 = 1
mirrorHalf 1 = 0
{-@ plusHalf :: SegmentHalf -> Maybe SegmentHalf @-}
plusHalf :: SegmentHalf -> Maybe SegmentHalf
plusHalf 0 = Just 1
plusHalf 1 = Nothing
{-@ minusHalf :: SegmentHalf -> Maybe SegmentHalf @-}
minusHalf :: SegmentHalf -> Maybe SegmentHalf
minusHalf 1 = Just 0
minusHalf 0 = Nothing
{-@ mirrorQuarter :: SegmentQuarter -> SegmentQuarter @-}
mirrorQuarter :: SegmentQuarter -> SegmentQuarter
mirrorQuarter n = mirrorHalf (quot n 2) * 2 + mirrorHalf (mod n 2)
{-@ mirrorEight :: SegmentEight -> SegmentEight @-}
mirrorEight :: SegmentEight -> SegmentEight
mirrorEight n = mirrorQuarter (quot n 2) * 2 + mirrorHalf (mod n 2)
{-@ plusQuarter :: SegmentQuarter -> Maybe SegmentQuarter @-}
plusQuarter :: SegmentQuarter -> Maybe SegmentQuarter
plusQuarter n = case (mod n 2) of
  0 -> Just $ n+1
  1 -> do {b <- plusHalf $ quot n 2; Just $ b*2}
{-@ minusQuarter :: SegmentQuarter -> Maybe SegmentQuarter @-}
minusQuarter :: SegmentQuarter -> Maybe SegmentQuarter
minusQuarter = fmap mirrorQuarter . plusQuarter.mirrorQuarter
{-@ plusEight :: SegmentEight -> Maybe SegmentEight @-}
plusEight :: SegmentEight -> Maybe SegmentEight
plusEight n = case (mod n 2) of
  0 -> Just $ n+1
  1 -> do {b <- plusQuarter $ quot n 2; Just $ b*2}
{-@ minusEight :: SegmentEight -> Maybe SegmentEight @-}
minusEight :: SegmentEight -> Maybe SegmentEight
minusEight = fmap mirrorEight . plusEight.mirrorEight
inw :: Rank -> Rank
inw 5 = 5
inw n = n+1
out :: Rank -> Maybe Rank
out 0 = Nothing
out n = Just $ n-1

{-@ type Pos = (Rank, File) @-}
type Pos = (Rank, File)
rank :: Pos -> Rank
rank (r, _) = r
file :: Pos -> File
file (_, f) = f

kfm :: SegmentEight
-- kfm = SegmentEight { segmentQuarter = SegmentQuarter { half = SecondHalf, halfQuarter = FirstHalf }, quarterHalf = FirstHalf }
kfm = 4 + 0 + 0
