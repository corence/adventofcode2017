
import Lib
import Data.Maybe

{-
0: 3
1: 2
4: 4
6: 4
-}

type Scanner = (Int, Int)

main :: IO ()
main = do
  -- part 1
  journeyScore scanners 0 & print
  journeyScore realScanners 0 & print

  -- part 2
  map (journeyCollisions scanners)     [0..] & takeWhile or & length & print
  map (journeyCollisions realScanners) [0..] & takeWhile or & length & print

journeyScore :: [Scanner] -> Int -> Int
journeyScore scanners delay = map (score delay) scanners & sum

score :: Int -> Scanner -> Int
score delay scanner
  = if willCollide delay scanner
        then magnitude scanner
        else 0

journeyCollisions :: [Scanner] -> Int -> [Bool]
journeyCollisions scanners delay = map (willCollide delay) scanners

willCollide :: Int -> Scanner -> Bool
willCollide delay (index, range) = (delay + index) `mod` ((range - 1) * 2) == 0

magnitude :: Scanner -> Int
magnitude = uncurry (*)

scanners = [
  (0, 3),
  (1, 2),
  (4, 4),
  (6, 4)
  ]

realScanners = [
  (0, 4),
  (1, 2),
  (2, 3),
  (4, 5),
  (6, 6),
  (8, 6),
  (10, 4),
  (12, 8),
  (14, 8),
  (16, 9),
  (18, 8),
  (20, 6),
  (22, 6),
  (24, 8),
  (26, 12),
  (28, 12),
  (30, 12),
  (32, 10),
  (34, 8),
  (36, 8),
  (38, 10),
  (40, 12),
  (42, 12),
  (44, 12),
  (46, 14),
  (48, 14),
  (50, 14),
  (52, 14),
  (54, 12),
  (56, 12),
  (58, 12),
  (60, 12),
  (62, 14),
  (64, 14),
  (66, 14),
  (68, 14),
  (70, 14),
  (80, 14),
  (82, 14),
  (86, 14),
  (88, 17),
  (94, 30),
  (98, 18)
  ]
