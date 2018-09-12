{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module ThreeManChess.Engine.Board where

-- import Control.Monad
import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.PosIterator
import ThreeManChess.Engine.Color

{-@ type Board a = Pos -> Maybe a @-}
type Board a = Pos -> Maybe a
newtype BoardWrap a = BoardWrap (Board a)

ourShowList :: (Show a) => [Maybe a] -> String
ourShowList xs = "[" ++ ourShowListHelper xs
ourShowListHelper :: (Show a) => [Maybe a] -> String
ourShowListHelper (Nothing:xs) = "___" ++ " " ++ ourShowListHelper xs
ourShowListHelper (Just x:xs) = show x ++ " " ++ ourShowListHelper xs
ourShowListHelper [] = "]"
ourShowListOfLists :: (Show a) => [[Maybe a]] -> String
ourShowListOfLists xs = "\n[" ++ ourShowListOfListsHelper xs
ourShowListOfListsHelper :: (Show a) => [[Maybe a]] -> String
ourShowListOfListsHelper (x:xs) = ourShowList x ++ "\n " ++ ourShowListOfListsHelper xs
ourShowListOfListsHelper [] = "] "

instance (Show a) => Show (BoardWrap a) where
  show (BoardWrap f) = ourShowListOfLists $ fmap (fmap f) allPos2D

put :: Board a -> Pos -> Maybe a -> Board a
put _ x what y | x==y = what
put before _ _ x = before x

swap :: Board a -> Pos -> Pos -> Board a
swap before to from x | x==to = before from
                      | x==from = before to
                      | otherwise = before x
empty :: Board a
empty _ = Nothing
-- starting :: Board a
-- starting (Pos 1 f) =

isEmpty :: Board a -> Pos -> Bool
isEmpty f p = case f p of
  Just _ -> False
  Nothing -> True

{-@ type ARank a = File -> Maybe a @-}
type ARank a = File -> Maybe a
{-@ type AFile a = Rank -> Maybe a @-}
type AFile a = Rank -> Maybe a
type TwoOf a = (a,a)
{-@ type ListOfTwo a = {v:[a] | len v == 2} @-}
{-@ type ListOfFour a = {v:[a] | len v == 4} @-}
{-@ type ListOfEight a = {v:[a] | len v == 8} @-}
{-@ _listToTwoOf :: ListOfTwo a -> TwoOf a @-}
_listToTwoOf :: [a] -> TwoOf a
_listToTwoOf [a,b] = (a,b)
_listToTwoOf _ = undefined
{-@ type ATwoOf a = SegmentHalf -> a @-}
type ATwoOf a = SegmentHalf ->  a
{-@ _ofTwoOf :: TwoOf a -> ATwoOf a @-}
_ofTwoOf :: TwoOf a -> ATwoOf a
_ofTwoOf x 0 = fst x
_ofTwoOf x 1 = snd x
{-@ type FourOf a = TwoOf (TwoOf a) @-}
type FourOf a = TwoOf (TwoOf a)
{-@ _listToFourOf :: ListOfFour a -> FourOf a @-}
_listToFourOf :: [a] -> FourOf a
_listToFourOf [a,b,c,d] = ((a,b),(c,d))
_listToFourOf _ = undefined
{-@ _ourMapTwo :: (a -> b) -> TwoOf a -> TwoOf b @-}
_ourMapTwo :: (a -> b) -> TwoOf a -> TwoOf b
_ourMapTwo f (a,b) = (f a, f b)
{-@ _ourMapFour :: (a -> b) -> FourOf a -> FourOf b @-}
_ourMapFour :: (a -> b) -> FourOf a -> FourOf b
_ourMapFour f = _ourMapTwo (_ourMapTwo f)
{-@ _ourMapEight :: (a -> b) -> EightOf a -> EightOf b @-}
_ourMapEight :: (a -> b) -> EightOf a -> EightOf b
_ourMapEight f = _ourMapTwo (_ourMapFour f)
-- {-@ _ourAnnotatedFmapListToTwoOf :: ListOfTwo (ListOfTwo a) -> ListOfTwo (TwoOf a) @-}
-- _ourAnnotatedFmapListToTwoOf = fmap _listToTwoOf
-- _ourAnnotatedFmapListToTwoOf [a,b] = [_listToTwoOf a, _listToTwoOf b]
{-@ _nestedListToFourOf :: ListOfTwo (ListOfTwo a) -> FourOf a @-}
_nestedListToFourOf :: [[a]] -> FourOf a
--_nestedListToFourOf = _listToTwoOf . _ourAnnotatedFmapListToTwoOf
_nestedListToFourOf = (_ourMapTwo _listToTwoOf) . _listToTwoOf
{-@ type AFourOf a = SegmentQuarter -> a @-}
type AFourOf a = SegmentQuarter -> a
{-@ _ofFourOf :: FourOf a -> AFourOf a @-}
_ofFourOf :: FourOf a -> AFourOf a
_ofFourOf x n = _ofTwoOf (_ofTwoOf x (quot n 2)) (mod n 2)
{-@ type EightOf a = TwoOf (FourOf a) @-}
type EightOf a = TwoOf (FourOf a)
{-@ _listToEightOf :: ListOfEight a -> EightOf a @-}
_listToEightOf :: [a] -> EightOf a
_listToEightOf [a,b,c,d,e,f,g,h] = (((a,b),(c,d)),((e,f),(g,h)))
{-@ _nestedListToEightOf :: ListOfTwo (ListOfTwo (ListOfTwo a)) -> EightOf a @-}
_nestedListToEightOf :: [[[a]]] -> EightOf a
-- _nestedListToEightOf =  _listToTwoOf . fmap _nestedListToFourOf
_nestedListToEightOf = (_ourMapTwo _nestedListToFourOf) . _listToTwoOf
{-@ _nestedOnceListToEightOf :: ListOfTwo (ListOfFour a) -> EightOf a @-}
_nestedOnceListToEightOf :: [[a]] -> EightOf a
-- _nestedOnceListToEightOf = _listToTwoOf . fmap _listToFourOf
_nestedOnceListToEightOf = (_ourMapTwo _listToFourOf) . _listToTwoOf
{-@ type AEightOf a = SegmentEight -> a @-}
type AEightOf a = SegmentEight -> a
{-@ _ofEightOf :: EightOf a -> AEightOf a @-}
_ofEightOf :: EightOf a -> AEightOf a
_ofEightOf x n = _ofTwoOf (_ofFourOf x (quot n 2)) (mod n 2)
type RankT a = (EightOf (Maybe a), EightOf (Maybe a), EightOf (Maybe a))
-- _listToRankT :: [a] -> RankT a
-- _listToARankHelper :: [Maybe a] -> Maybe Count -> ARank a
-- _listToARankHelper (x:_) Nothing (File White (SegmentEight (SegmentQuarter FirstHalf FirstHalf) FirstHalf)) = x
-- _listToARankHelper [x] (File Black (SegmentEight (SegmentQuarter SecondHalf SecondHalf) SecondHalf)) = x
-- _listToARankHelper [_] _ = undefined
-- _listToARankHelper [] _ = undefined
-- _listToARankHelper (_:xs) f = _listToARankHelper xs (plus f)
filterFirst :: (a -> Bool) -> [a] -> Maybe a
filterFirst f (x:xs) = if f x then Just x else filterFirst f xs
filterFirst _ [] = Nothing
listToARank :: [Maybe a] -> ARank a
-- listToARank (x:_) (File White (SegmentEight (SegmentQuarter FirstHalf FirstHalf) FirstHalf)) = x
-- listToARank (_:xs) f = _listToARankHelper xs f --maybe we need a lenght24 guard?
-- listToARank [] _ = undefined
listToARank x f = snd =<< filterFirst (\a -> fst a == f) (zip allFilesFromZero x)
-- _listOfListsToBoardHelper :: [[Maybe a]] -> Board a
-- _listOfListsToBoardHelper (x:_) (MostOuter, f) = listToARank x f
_giveByRankHelper :: [[Maybe a]] -> Rank -> Maybe [Maybe a]
_giveByRankHelper x r = fmap snd (filterFirst (\a -> fst a == r) (zip ranks x))
listOfListsToBoard :: [[Maybe a]] -> Board a
listOfListsToBoard x (r,f) = _giveByRankHelper x r >>= flip listToARank f

_ofRankTColorSegm :: RankT a -> Color -> EightOf (Maybe a)
_ofRankTColorSegm (x,_,_) White = x
_ofRankTColorSegm (_,x,_) Gray = x
_ofRankTColorSegm (_,_,x) Black = x
{-@_ofRankT :: RankT a -> ARank a @-}
_ofRankT :: RankT a -> ARank a
_ofRankT x n = _ofEightOf (_ofRankTColorSegm x (segmColor n)) (segmFile n)
{-@ type BoardT a = (RankT a, RankT a, RankT a, RankT a, RankT a, RankT a) @-}
type BoardT a = (RankT a, RankT a, RankT a, RankT a, RankT a, RankT a)
{-@ _ofBoardTRankT :: BoardT a -> Rank -> RankT a @-}
_ofBoardTRankT :: BoardT a -> Rank -> RankT a
_ofBoardTRankT (x,_,_,_,_,_) 0 = x
_ofBoardTRankT (_,x,_,_,_,_) 1 = x
_ofBoardTRankT (_,_,x,_,_,_) 2 = x
_ofBoardTRankT (_,_,_,x,_,_) 3 = x
_ofBoardTRankT (_,_,_,_,x,_) 4 = x
_ofBoardTRankT (_,_,_,_,_,x) 5 = x
{-@ _ofBoardT :: BoardT a -> Board a @-}
_ofBoardT :: BoardT a -> Board a
_ofBoardT x (r, f) = _ofRankT (_ofBoardTRankT x r) f

data BoardSingleChange a = MoveFromToOverwriting Pos Pos | DoubleMoveFromToOverwriting (Pos,Pos) (Pos,Pos) |
                           MoveFromToOverwritingWithOtherDisappear (Pos,Pos) Pos | MoveWithReplacement (Pos,Pos) a
performSingleChange :: BoardSingleChange a -> Board a -> Board a
performSingleChange (MoveFromToOverwriting a b) f = put (swap f a b) a Nothing
performSingleChange (DoubleMoveFromToOverwriting a b) f = performSingleChanges [uncurry MoveFromToOverwriting a, uncurry MoveFromToOverwriting b] f
performSingleChange (MoveFromToOverwritingWithOtherDisappear a b) f = put (performSingleChange (uncurry MoveFromToOverwriting a) f) b Nothing
performSingleChange (MoveWithReplacement (wf,wt) a) f = put (put f wt $ Just a) wf Nothing  --put f w (Just a)
performSingleChanges :: [BoardSingleChange a] -> Board a -> Board a
performSingleChanges xs b = foldl (flip performSingleChange) b xs

checkEmpties :: Board a -> [Pos] -> Bool
checkEmpties f (x:xs) = isEmpty f x && checkEmpties f xs
checkEmpties _ [] = True
