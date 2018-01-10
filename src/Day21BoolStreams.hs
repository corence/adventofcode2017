
module Day21BoolStreams where

import Lib
import qualified Data.List as List

toRows :: Int -> [a] -> [[a]]
toRows rowLength [] = []
toRows rowLength pixels
  = List.splitAt rowLength pixels
  & (\(row, other) -> row : toRows rowLength other)

fromRows :: [[a]] -> [a]
fromRows = concat

toCols :: Int -> [a] -> [[a]]
toCols rowLength [] = []
toCols rowLength pixels
  = pixels -- [0,1,2,3,4,5,6,7,8]
  & toRows rowLength -- [[0,1,2],[3,4,5],[6,7,8]]
  & transpose

-- rows to cols, or cols to rows
transpose :: [[a]] -> [[a]]
transpose pixels
  = pixels                       --  [[0,1,2],[3,4,5],[6,7,8]]
  & iterate (map (drop 1))       -- [[[0,1,2],[3,4,5],[6,7,8]], [[1,2],[4,5],[7,8]], [[2],[5],[8]], [[],[],[]]...]
  & map (takeWhile (not . null)) -- [[[0,1,2],[3,4,5],[6,7,8]], [[1,2],[4,5],[7,8]], [[2],[5],[8]], []...]
  & takeWhile (not . null)       -- [[[0,1,2],[3,4,5],[6,7,8]], [[1,2],[4,5],[7,8]], [[2],[5],[8]]]
  & map (map head)               -- [[0,3,6],                    [1,4,7],             [2,5,8]]

fromCols :: Int -> [[a]] -> [a]
fromCols rowLength cols
  = cols -- [[0,3,6],[1,4,7],[2,5,8]]
  & transpose -- [[0,1,2],[3,4,5],[6,7,8]]
  & fromRows

-- separate the grid into squares of "subLength" width and height
toSubgrids :: Int -> Int -> [a] -> [[a]]
toSubgrids gridLength subLength pixels
  = pixels
  & toRows (gridLength * subLength)
  & map (toRows subLength)
  & map (toCols (gridLength `div` subLength))
  & map (map concat)
  & concat

fromSubgrids :: Int -> Int -> [[a]] -> [a]
fromSubgrids gridLength subLength subgrids
  = subgrids
  & toRows (gridLength `div` subLength)
  & map (map (toRows subLength))
  & map (fromCols (gridLength `div` subLength))
  & concat
  & concat

flipHorz :: Int -> [a] -> [a]
flipHorz rowLength pixels
  = pixels
  & toRows rowLength
  & map reverse
  & fromRows

flipVert :: Int -> [a] -> [a]
flipVert rowLength pixels
  = pixels
  & toCols rowLength
  & map reverse
  & fromCols rowLength

transposeGrid :: Int -> [a] -> [a]
transposeGrid rowLength pixels
  = pixels
  & toRows rowLength
  & fromCols rowLength

permeate :: Int -> [a] -> [[a]]
permeate rowLength pixels
  = [pixels
  , pixels & fh
  , pixels & fv
  , pixels & fh & fv
  , pixels & tr
  , pixels & tr & fh
  , pixels & tr & fv
  , pixels & tr & fh & fv]
  where fh = flipHorz rowLength
        fv = flipVert rowLength
        tr = transposeGrid rowLength
