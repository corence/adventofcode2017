
module Hex where

import qualified Data.Map as Map
import Data.Map(Map, (!))

type Pos = Map Dir Int

origin :: Pos
origin = Map.empty

data Dir = NE | N | NW | SE | S | SW
           deriving (Eq, Ord)

go :: Dir -> Pos -> Pos
go dir = Map.insertWith (+) dir 1

distanceFromOrigin :: Pos -> Int
distanceFromOrigin pos = north + northwest + northeast
  where north = abs (pos ! N - pos ! S)
        northeast = abs (pos ! NE - pos ! SW)
        northwest = abs (pos ! NW - pos ! SE)

followPath :: Pos -> [Dir] -> Pos
followPath = foldr go
