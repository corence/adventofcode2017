
import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Lib

starter = "abcdefghijklmnop"

main :: IO ()
main = do
  input <- readFile "inputs/input16.txt" <&> lines <&> head
  let instructions = actuallyParse parseDance input
  let result = foldl' (&) starter instructions
  putStrLn result
  let result2 = starter & iterate (\initial -> foldl' (&) initial instructions) & take 5
  print result2
  let rearrange = rearrangeVia (findIndexes result)
  let result3 = result & iterate rearrange & take 5
  print result3
  print (rearrange starter)

-- "abcd" "bdca" -> [!! 1, !! 3, !! 2, !! 0]

findIndexes :: String -> [Int]
findIndexes string
  = string
  & map ord
  & map (subtract (ord 'a'))

rearrangeVia :: [Int] -> String -> String
rearrangeVia indexes string
  = map (string !!) indexes

spin :: Int -> String -> String
spin num string = drop (length string - num) string ++ take (length string - num) string

exchange :: Int -> Int -> String -> String
exchange index1 index2 string
  | index1 == index2 = string
  | index1 > index2 = exchange index2 index1 string
  | otherwise = prelude ++ [char2] ++ intermission ++ [char1] ++ finale
    where char1 = string !! index1
          char2 = string !! index2
          prelude = take index1 string
          intermission = take (index2 - index1 - 1) (drop (index1 + 1) string)
          finale = drop (index2 + 1) string

partner :: Char -> Char -> String -> String
partner _ _ [] = []
partner char1 char2 (c:string)
  | c == char1 = char2 : next
  | c == char2 = char1 : next
  | otherwise = c : next
  where next = partner char1 char2 string

parseDance :: ReadP [String -> String]
parseDance = sepBy parseInstruction (char ',')

parseInstruction :: ReadP (String -> String)
parseInstruction = parseSpin <++ parseExchange <++ parsePartner

parseSpin :: ReadP (String -> String)
parseSpin = do
  char 's'
  num <- parseInt
  pure $ spin num

parseExchange :: ReadP (String -> String)
parseExchange = do
  char 'x'
  index1 <- parseInt
  char '/'
  index2 <- parseInt
  pure $ exchange index1 index2

parsePartner :: ReadP (String -> String)
parsePartner = do
  char 'p'
  char1 <- get
  char '/'
  char2 <- get
  pure $ partner char1 char2
