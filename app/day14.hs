
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
  -- deconstructing part 1
  "flqrgnkx" & print
  putStrLn "-1"
  "flqrgnkx" & makeCodes & length & print
  "flqrgnkx" & makeCodes & head & print
  putStrLn "-2"
  "flqrgnkx" & makeCodes & map knotHash & head & print
  putStrLn "-3"
  "flqrgnkx" & makeCodes & map knotHash & map hexToBinary & length & print
  "flqrgnkx" & makeCodes & map knotHash & map hexToBinary & head & print
  putStrLn "-4"
  "flqrgnkx" & makeCodes & map knotHash & map hexToBinary & concat & length & print
  putStrLn "-5"
  "flqrgnkx" & makeCodes & map knotHash & map hexToBinary & concat & binaryToBools & take 10 & print
  putStrLn "-6"
  "flqrgnkx" & makeCodes & map knotHash & map hexToBinary & concat & binaryToBools & boolsToIndices & length & print
  "flqrgnkx" & makeCodes & map knotHash & map hexToBinary & concat & binaryToBools & boolsToIndices & take 10 & print
  putStrLn "-7"
  "flqrgnkx" & makeCodes & map knotHash & map hexToBinary & concat & binaryToBools & boolsToIndices & indicesToRegions 128 & Map.size & print

  -- part 1
  putStrLn "---"
  "flqrgnkx" & keyToBools & boolsToRegions 128 & Map.keys & length & print
  "jzgqcdpd" & keyToBools & boolsToRegions 128 & Map.keys & length & print

  -- part 2 sample
  putStrLn "---"
  let part2TestData1 = binaryToBools $
                          "0111"
                       ++ "0101"
                       ++ "1010"
                       ++ "1111"
  let part2TestData2 rowLength = cycle [True, False] & take rowLength & map (\v -> iterate not v & take rowLength) & concat
  let part2TestData3 = binaryToBools $
                          "0000000"
                       ++ "0111000"
                       ++ "0101000"
                       ++ "0000000"
                       ++ "0001010"
                       ++ "0001110"
                       ++ "0000000"
  let part2TestData4 = binaryToBools $
                          "01"
                       ++ "10"

  putStr "expected: 2, actual: " >> part2TestData1 & boolsToRegions 4 & countRegions & print
  part2TestData2 4 & boolsToRegions 4 & Map.showTree & putStrLn
  part2TestData2 4 & boolsToIndices & print
  putStr "expected: 8, actual: " >> part2TestData2 4 & boolsToRegions 4 & countRegions & print
  putStr "expected: 2, actual: " >> part2TestData3 & boolsToRegions 7 & countRegions & print
  putStr "expected: 2, actual: " >> part2TestData4 & boolsToRegions 2 & countRegions & print

  -- part 2
  putStrLn "---"
  "flqrgnkx" & keyToBools & boolsToRegions 128 & countRegions & print
  "jzgqcdpd" & keyToBools & boolsToRegions 128 & countRegions & print
  putStrLn "(2122 is too high)"

keyToBools :: String -> [Bool]
keyToBools
  = binaryToBools
  . concat
  . map hexToBinary
  . map knotHash
  . makeCodes

boolsToRegions :: Int -> [Bool] -> Map Int Int
boolsToRegions rowLength
  = indicesToRegions rowLength
  . boolsToIndices

boolsToIndices :: [Bool] -> [Int]
boolsToIndices values = zip [0..] values & filter snd & map fst

binaryToBools :: String -> [Bool]
binaryToBools = map (== '1')

indicesToRegions :: Int -> [Int] -> Map Int Int
indicesToRegions rowLength = foldl' (addPixel rowLength) Map.empty

countRegions :: Map Int Int -> Int
countRegions = Set.size . Set.fromList . Map.elems

-- TODO: i hate this method; it would be great to refactor it to be much longer so that it's clear
addPixel :: Int -> Map Int Int -> Int -> Map Int Int
addPixel rowLength regions index = Map.insert index (head possibleValues) regions
  where possibleValues = catMaybes [
          if index `mod` rowLength == 0 then Nothing else Map.lookup (index - 1) regions,
          if index + 1 `mod` rowLength == 0 then Nothing else Map.lookup (index + 1) regions,
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
