
import Day21BoolStreams
import Day21PatternMatcher
import Lib

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
    & take 1
    & map pPixels
    & map show
    & unlines
    & print

  matchers <- load
  iterate (enhance matchers) initialPattern
    & (!! 5)
    & pPixels
    & filter id
    & length
    & print
