
module NumericConversions where

import Data.Char
import Data.List(foldl')

parseInt :: Int -> String -> Int
parseInt base string = foldl' (augment base) 0 (map digitToInt string)
  where augment base total value
          | value >= base = error $ "hey you can't have a value higher than the base but you had parseInt " ++ show base ++ " " ++ string
          | otherwise = total * base + value
