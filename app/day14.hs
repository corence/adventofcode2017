
import Day10Rope
import Lib
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Maybe(catMaybes)
import Data.List(foldl')

makeCodes :: String -> [String]
makeCodes input = [0..127] & map ((input ++) . ("-" ++) . show)

main :: IO ()
main = do
  -- part 1
  "flqrgnkx" & keyToGridIndices & length & print
  "jzgqcdpd" & keyToGridIndices & length & print

  -- part 2
  "flqrgnkx" & keyToRegions & countRegions & print
  "jzgqcdpd" & keyToRegions & countRegions & print

keyToRegions :: String -> Map Int Int
keyToRegions = indicesToRegions . keyToGridIndices

keyToGridIndices :: String -> [Int]
keyToGridIndices input = input & makeCodes & makeHashes & makeBoolsGrid & getIndices
  where getIndices values = zip [0..] values & filter snd & map fst
        makeHashes = map (hexToBinary . knotHash)
        makeBoolsGrid = map (== '1') . concat

indicesToRegions :: [Int] -> Map Int Int
indicesToRegions = foldl' (addPixel 128) Map.empty

countRegions :: Map Int Int -> Int
countRegions = Set.size . Set.fromList . Map.elems

addPixel :: Int -> Map Int Int -> Int -> Map Int Int
addPixel rowLength regions index = Map.insert index (head possibleValues) regions
  where possibleValues = catMaybes [
          Map.lookup (index - 1) regions,
          Map.lookup (index + 1) regions,
          Map.lookup (index - rowLength) regions,
          Map.lookup (index + rowLength) regions,
          Just index
          ]

-- something something printf but i'm on the plane so i have to write whatever will work for now and refactor this later
hexToBinary :: String -> String
hexToBinary hex = map toBinChar hex & concat
  where toBinChar '0' = "0000"
        toBinChar '1' = "0001"
        toBinChar '2' = "0010"
        toBinChar '3' = "0011"
        toBinChar '4' = "0100"
        toBinChar '5' = "0101"
        toBinChar '6' = "0110"
        toBinChar '7' = "0111"
        toBinChar '8' = "1000"
        toBinChar '9' = "1001"
        toBinChar 'a' = "1010"
        toBinChar 'b' = "1011"
        toBinChar 'c' = "1100"
        toBinChar 'd' = "1101"
        toBinChar 'e' = "1110"
        toBinChar 'f' = "1111"
        toBinChar char = error $ "i cant really interpret the hex " ++ [char] ++ " from hex [" ++ hex ++ "]"

-- For example, if your key string were flqrgnkx, then the first row would be given by the bits of the
-- knot hash of flqrgnkx-0, the second row from the bits of the knot hash of flqrgnkx-1,
-- and so on until the last row, flqrgnkx-127.

-- The output of a knot hash is traditionally represented by 32 hexadecimal digits;
-- each of these digits correspond to 4 bits, for a total of 4 * 32 = 128 bits.
-- To convert to bits, turn each hexadecimal digit to its equivalent binary value, high-bit first:
-- 0 becomes 0000, 1 becomes 0001, e becomes 1110, f becomes 1111, and so on;
-- a hash that begins with a0c2017... in hexadecimal would begin with 10100000110000100000000101110000... in binary.

-- In this example, 8108 squares are used across the entire 128x128 grid.

-- Given your actual key string, how many squares are used?
