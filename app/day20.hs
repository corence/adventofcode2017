
import Text.ParserCombinators.ReadP
import Lib
import Day20Particles

main :: IO ()
main = readFile "inputs/input20.txt"
     <&> lines
     <&> zip [0..]
     <&> map (\(num, input) -> actuallyParse (parseParticle num) input)
     <&> originest
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
