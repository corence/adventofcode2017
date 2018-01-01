
module Day9Groups where

import Lib
import Text.ParserCombinators.ReadP
import Control.Applicative((<|>))

data Entry = Group [Entry] | Garbage

countGroups :: Entry -> Int
countGroups Garbage = 0
countGroups (Group entries)
  = map countGroups entries & sum & (+ 1)

parseCountGroups :: String -> Int
parseCountGroups input
 = actuallyParse parseEntry input & countGroups

scoreGroups :: Int -> Entry -> Int
scoreGroups _ Garbage = 0
scoreGroups depth (Group entries)
  = map (scoreGroups (depth + 1)) entries & sum & (+ depth)

score :: String -> Int
score input
  = actuallyParse parseEntry input & scoreGroups 1

parseEntry :: ReadP Entry
parseEntry = parseGarbage <|> parseGroup

parseGroup :: ReadP Entry
parseGroup = do
  char '{'
  entries <- sepBy parseEntry (char ',')
  char '}'
  skipSpaces
  pure $ Group entries

parseGarbage :: ReadP Entry
parseGarbage = do
  char '<'
  many character
  char '>'
  skipSpaces
  pure Garbage
  where character = (char '!' >> get) <++ satisfy (/= '>')
