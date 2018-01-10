
import Day21BoolStreams
import Day21PatternMatcher
import Lib
import Data.List(intercalate)

main :: IO ()
main = do
  -- test
  iterate (enhance testMatchers) initialPattern
    & take 3
    & map pPixels
    & map show
    & map lines
    & print

  iterate (enhance testMatchers) initialPattern
    & (!! 2)
    & pPixels
    & filter id
    & length
    & print

  matchers <- load
  iterate (enhance matchers) initialPattern
    & take 6
    & map drawPattern
    & intercalate "\n\n"
    & putStrLn

  matchers <- load
  iterate (enhance matchers) initialPattern
    & take 15
    & map pPixels
    & map (filter id)
    & map length
    & print

  putStrLn "part1 is higher than 149."
