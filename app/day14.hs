
import Day10Rope
import Lib
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Maybe(catMaybes)
import Data.List(foldl')
import Data.Bifunctor

type Region = Set Pos
type Pos = (Int, Int)

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
  --"flqrgnkx" & makeCodes & map knotHash & map hexToBinary & concat & binaryToBools & boolsToIndices & indicesToRegions 128 & Map.size & print

  -- part 1
  putStrLn "---"
  putStrLn "part1 example must be 8108:"
  "flqrgnkx" & keyToBools & boolsToIndices & length & print
  putStrLn "part1 real answer is 8074:"
  "jzgqcdpd" & keyToBools & boolsToIndices & length & print

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

  putStr "expected: 2, actual: " >> part2TestData1 & boolsToRegions 4 & length & print
  part2TestData2 4 & boolsToIndices & print
  putStr "expected: 8, actual: " >> part2TestData2 4 & boolsToRegions 4 & length & print
  putStr "expected: 2, actual: " >> part2TestData3 & boolsToRegions 7 & length & print
  putStr "expected: 2, actual: " >> part2TestData4 & boolsToRegions 2 & length & print

  -- part 2
  putStrLn "---"
  putStrLn "part2 example is 1242:"
  "flqrgnkx" & keyToBools & boolsToRegions 128 & length & print
  putStrLn "part2 real answer:"
  "jzgqcdpd" & keyToBools & boolsToRegions 128 & length & print
  putStrLn "(2122 is too high)"

keyToBools :: String -> [Bool]
keyToBools
  = binaryToBools
  . concat
  . map hexToBinary
  . map knotHash
  . makeCodes

boolsToRegions :: Int -> [Bool] -> [Region]
boolsToRegions rowLength
  = posesToRegions
  . Set.fromList
  . map (indexToPos rowLength)
  . boolsToIndices

indexToPos :: Int -> Int -> (Int, Int)
indexToPos rowLength index = (index `mod` rowLength, index `div` rowLength)

posesToRegions :: Set Pos -> [Region]
posesToRegions poses
  | Set.null poses = []
  | otherwise = extract (getOne poses) poses & next
  where getOne = head . Set.toList
        next (group, newPoses) = Set.fromList group : posesToRegions newPoses

extract :: Pos -> Set Pos -> ([Pos], Set Pos)
extract pos poses
  | Set.member pos poses
      = ([pos], Set.delete pos poses)
      & extractAt (first (+ 1))
      & extractAt (first (subtract 1))
      & extractAt (second (+ 1))
      & extractAt (second (subtract 1))
  | otherwise = ([], poses)
  where extractAt mutate (results, poses) = extract (mutate pos) poses & first (++ results)

extractRegion :: Pos -> Set Pos -> ([Region], Set Pos)
extractRegion pos@(x, y) poses
  = foldl' tryAddPos ([Set.singleton pos], Set.delete pos poses) targets
      where targets = [(x + 1, y),
                       (x - 1, y),
                       (x, y + 1),
                       (x, y - 1)]
            tryAddPos (regions, poses) pos2
                        | Set.member pos2 poses = extractRegion pos2 poses & first (++ regions)
                        | otherwise = (regions, poses)

boolsToIndices :: [Bool] -> [Int]
boolsToIndices values = zip [0..] values & filter snd & map fst

binaryToBools :: String -> [Bool]
binaryToBools = map (== '1')

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
