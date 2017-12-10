{-# Language DeriveDataTypeable #-}
module ThreeManChess.Engine.Color where

import Data.Data
import Data.Set (Set)

data Color = White | Gray | Black deriving (Eq, Show, Data, Typeable)
colorSet = Set.fromList (White, Gray, Black)
