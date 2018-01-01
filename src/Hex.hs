
module Hex where

import qualified Data.Map as Map
import Data.Map(Map)
import Lib

import Debug.Trace

type Pos = Map Dir Int

data Dir = NW | N | NE | SE | S | SW deriving (Eq, Ord, Show)
allDirs = [NW, N, NE, SE, S, SW]

origin :: Pos
origin = Map.empty

normalize :: Pos -> Pos
normalize pos
  = if pos == pos2 -- ewwww. but we need it because otherwise these operations are order-sensitive.
      then pos2
      else normalize pos2
    where pos2 = pos
            & transmute N NE NW
            & transmute S SE SW
            & transmute NE N SE
            & transmute NW N SW
            & transmute SE S NE
            & transmute SW S NW
            & cancel N S
            & cancel NW SE
            & cancel NE SW

-- the given 2 directions cancel each other out
-- one of the directions is guaranteed to be zero after this
cancel :: Dir -> Dir -> Pos -> Pos
cancel dir1 dir2 = commute [] [dir1, dir2]

-- the "target" direction absorbs whatever shared value the two "source" directions have
-- for example, 2 northeast + 2 south becomes 2 southeast
transmute :: Dir -> Dir -> Dir -> Pos -> Pos
transmute target source1 source2 = commute [target] [source1, source2]

-- the "target" direction absorbs whatever shared value the two "source" directions have
-- for example, 2 northeast + 2 south becomes 2 southeast
commute :: [Dir] -> [Dir] -> Pos -> Pos
commute targets sources pos
  = pos
  & (\pos -> foldr (sub minValue) pos sources)
  & (\pos -> foldr (add minValue) pos targets)
  where add amount dir = Map.insertWith (+) dir amount
        sub amount = Map.update (Just . subtract amount)
        minValue = map value sources & minimum
        value dir = Map.findWithDefault 0 dir pos

distanceFromOrigin :: Pos -> Int
distanceFromOrigin = sumValues allDirs . normalize

sumValues :: (Ord k, Num v) => [k] -> Map k v -> v
sumValues keys mappy = map (\key -> Map.findWithDefault 0 key mappy) keys & sum

go :: Dir -> Pos -> Pos
go dir = Map.insertWith (+) dir 1

followPath :: Pos -> [Dir] -> Pos
followPath = foldr go
