
import Lib
import Data.Maybe

{-
0: 3
1: 2
4: 4
6: 4
-}

-- depth, range, pos.
-- pos goes from 0 (inclusive) to 2 * range (exclusive)
type Scanner = (Int, Int, Int)
type Damage = (Int, Int)

scanners = [
  (0, 3, 0),
  (1, 2, 0),
  (4, 4, 0),
  (6, 4, 0)
  ] :: [Scanner]

damageMagnitude :: Damage -> Int
damageMagnitude = uncurry (*)

scannerDamage :: Scanner -> Maybe Damage
scannerDamage (depth, range, 0) = Just (depth, range)
scannerDamage _ = Nothing

tripDamage :: Int -> [Scanner] -> [Damage]
tripDamage _ [] = []
tripDamage index (scanner@(depth, range, pos) : scanners)
  = if index == depth
        then maybeToList (scannerDamage scanner) ++ tripDamage (index + 1) (map advance scanners)
        else tripDamage (index + 1) (map advance (scanner:scanners))

advance :: Scanner -> Scanner
advance (depth, range, pos)
  = (depth,
     range,
     (pos + 1) `mod` ((range - 1) * 2))

main :: IO ()
main = do
  -- part 1
  print $ tripDamage 0 realScanners

  -- part 2
  map (tripDamageWithDelay realScanners) [0..] & takeWhile (> 0) & length & print

tripDamageWithDelay :: [Scanner] -> Int -> Int
tripDamageWithDelay scanners delay = tripDamagesWithDelay scanners delay & map damageMagnitude & take (length scanners) & sum

tripDamagesWithDelay :: [Scanner] -> Int -> [Damage]
tripDamagesWithDelay scanners delay = delayTrip delay scanners & tripDamage 0

delayTrip :: Int -> [Scanner] -> [Scanner]
delayTrip delay scanners = scanners & iterate (map advance) & (!! delay)

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
  ] & map (\(x, y) -> (x, y, 0)) :: [Scanner]
