
import Lib
import Hex
import Text.ParserCombinators.ReadP
import Data.Char

main :: IO ()
main = do
  sourceText <- readFile "input11.txt" <&> lines <&> head -- just get the first line of input
  let dirs = actuallyParse parseDirs sourceText
  print $ length dirs

  -- part 1
  let pos = followPath origin dirs
  print pos
  print (normalize pos)
  print $ distanceFromOrigin pos

  -- part 2
  let poses = plotPath origin dirs
  let biggestDistance = map distanceFromOrigin poses & maximum
  print biggestDistance

parseDirs :: ReadP [Dir]
parseDirs = do
  strings <- sepBy (many1 $ satisfy isAlpha) (char ',')
  pure $ map stringToDir strings

stringToDir :: String -> Dir
stringToDir  "n" =  N
stringToDir "ne" = NE
stringToDir "nw" = NW
stringToDir "se" = SE
stringToDir  "s" =  S
stringToDir "sw" = SW
