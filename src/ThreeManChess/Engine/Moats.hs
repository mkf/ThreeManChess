module ThreeManChess.Engine.Moats where

import ThreeManChess.Engine.Color
import ThreeManChess.Engine.Pos
import ThreeManChess.Engine.Possibilities

data BridgeState = Bridged | Unbridged deriving (Eq, Read, Show)

type MoatsState = (BridgeState, BridgeState, BridgeState)
type ThreeBools = (Bool,Bool,Bool)

threeMap :: (a -> b) -> (a,a,a) -> (b,b,b)
threeMap f (a,b,c) = (f a, f b, f c)

fromBool :: Bool -> BridgeState
fromBool a = if a then Bridged else Unbridged
toBool :: BridgeState -> Bool
toBool Bridged = True
toBool Unbridged = False

fromBools :: ThreeBools -> MoatsState
fromBools = threeMap fromBool
toBools :: MoatsState -> ThreeBools
toBools = threeMap toBool

isBridgedDirec :: FilewiseDirection -> MoatsState -> Color -> BridgeState
isBridgedDirec Pluswards (x,_,_) Black = x
isBridgedDirec Pluswards (_,x,_) White = x
isBridgedDirec Pluswards (_,_,x) Gray = x
isBridgedDirec Minuswards x c = isBridgedDirec Pluswards x (prev c)

isBridgedBetween :: MoatsState -> (Color,Color) -> BridgeState
isBridgedBetween x (a,b) | b == next a = isBridgedDirec Pluswards x a
                         | otherwise = isBridgedDirec Minuswards x a

withSetDirec :: BridgeState -> FilewiseDirection -> MoatsState -> Color -> MoatsState
withSetDirec z Pluswards (_,x,y) Black = (z,x,y)
withSetDirec z Pluswards (x,_,y) White = (x,z,y)
withSetDirec z Pluswards (x,y,_) Gray = (x,y,z)
withSetDirec z Minuswards x c = withSetDirec z Pluswards x (prev c)

withSetBetween :: BridgeState -> MoatsState -> (Color,Color) -> MoatsState
withSetBetween z x (a,b) | b == next a = withSetDirec z Pluswards x a
                         | otherwise = withSetDirec z Minuswards x a

allBridged :: MoatsState
allBridged = (Bridged, Bridged, Bridged)
noBridges :: MoatsState
noBridges = (Unbridged, Unbridged, Unbridged)
