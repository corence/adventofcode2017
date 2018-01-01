
import Text.ParserCombinators.ReadP
import Lib
import Day20Particles

main :: IO ()
main = part1 >> part2

part1 :: IO ()
part1 = readFile "inputs/input20.txt"
     <&> lines
     <&> zip [0..]
     <&> map (\(num, input) -> actuallyParse (parseParticle num) input)
     <&> originest
     >>= print

-- part2:
-- at any given time, if a particle has the greatest (or lowest) position AND velocity AND acceleration in a single axis THEN it is untouchable and can never collide, so we eliminate it
-- so each frame, we must:
-- * check for collisions
-- * check for offsides
-- * check for emptiness
-- i believe we need 9 separate Map data structures to track the position, then everything can be straightforward

-- but, counterexample:
-- p=<5,0,0> v=<0,1,0> a=<0,0,0>
-- p=<3,0,0> v=<0,1,0> a=<0,0,0>
-- hmm, so equal-lowest = lowest! need a multimap i guess?

-- bah, we don't need multimaps, we can just recompute it every frame -- it won't cost more time than it already would

part2 :: IO ()
part2 = readFile "inputs/input20.txt"
     <&> lines
     <&> zip [0..]
     <&> map (\(num, input) -> actuallyParse (parseParticle num) input)
     <&> calculateDivergers
     <&> length
     >>= print

parseParticle :: Int -> ReadP Particle
parseParticle index = do
  string "p="
  pos <- parseVector
  string ", v="
  vel <- parseVector
  string ", a="
  acc <- parseVector
  pure $ Particle index pos vel acc

parseVector :: ReadP Vec3
parseVector = do
  char '<'
  x <- parseInt
  char ','
  y <- parseInt
  char ','
  z <- parseInt
  char '>'
  pure $ Vec3 x y z
