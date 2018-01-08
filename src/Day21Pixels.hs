
module Day21Pixels where

import Lib
import qualified Data.List as List

type Pos = (Int, Int)
data Grid = Grid { _offset :: Pos, _blacks :: [Pos] } deriving (Eq, Ord)

split :: Int -> Grid -> [Grid]
split newSize (Grid _ poses)
  = offsets
  & map makeGrid
  & (\grids -> List.foldr populate grids poses)
  where offsets = [(x, y) | x <- [0..(newSize - 1)], y <- [0..(newSize - 1)]]
        makeGrid offset = Grid offset []
        populate pos [] = error "nope"
        populate pos@(x, y) (grid@(Grid offset poses) : grids)
          = if (x `mod` newSize, y `mod` newSize) == offset
              then Grid offset (pos : poses) : grids
              else grid : populate pos grids

unsplit :: [Grid] -> Grid
unsplit grids = Grid minOffset allPoses
  where minX = grids & map _offset & map fst & minimum
        minY = grids & map _offset & map snd & minimum
        minOffset = (minX, minY)
        allPoses = grids & map _blacks & concat

match :: Grid -> Grid -> Bool
match grid1 grid2 = normalize grid1 == normalize grid2

normalize :: Grid -> Grid
normalize (Grid (ox, oy) blacks) = Grid (0, 0) (map normalizePos blacks)
  where normalizePos (x, y) = (x - ox, y - oy)

-- there are 8:
-- input
-- flip x
-- flip y
-- flip both
-- rotate
-- rotate + flip x
-- rotate + flip y
-- rotate + flip both
permeations :: Int -> Grid -> [Grid]
permeations size (Grid offset blacks) = do
  rotate <- [False, True]
  flipX <- [False, True]
  flipY <- [False, True]
  let result = transform rotate flipX flipY blacks
      
