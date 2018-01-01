
module Day15Generator where

import Data.Bits
import Lib

factorA = 16807 :: Int
factorB = 48271 :: Int

stream :: Int -> Int -> [Int]
stream factor initial = iterate (step factor) initial & drop 1 & take 40000000

-- The generators both work on the same principle.
-- To create its next value, a generator will take the previous value it produced,
-- multiply it by a factor (generator A uses 16807; generator B uses 48271),
-- and then keep the remainder of dividing that resulting product by 2147483647.
-- That final remainder is the value it produces next.
step :: Int -> Int -> Int
step factor lastNum = factor * lastNum `mod` 2147483647

-- return true if the lowest 16 bits of both numbers match
match :: Int -> Int -> Bool
match a b = a .&. 65535 == b .&. 65535

countMatches :: [Int] -> [Int] -> Int
countMatches stream1 stream2 = zipWith match stream1 stream2 & filter id & length
