
module Day15Generator where

factorA = 16807 :: Int
factorB = 48271 :: Int

-- The generators both work on the same principle.
-- To create its next value, a generator will take the previous value it produced,
-- multiply it by a factor (generator A uses 16807; generator B uses 48271),
-- and then keep the remainder of dividing that resulting product by 2147483647.
-- That final remainder is the value it produces next.
step :: Int -> Int -> Int
step factor lastNum = factor * lastNum `mod` 2147483647
