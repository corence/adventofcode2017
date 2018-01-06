
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Lib
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import Control.Monad.ST
import Data.Maybe

starter :: V.Vector Char
starter = V.fromList "abcdefghijklmnop"

type Instruction s = M.IOVector Char -> IO ()

main :: IO ()
main = do
  input <- readFile "inputs/input16.txt" <&> lines <&> head
  let instructions = actuallyParse parseDance input

  output <- doInstructions instructions starter
  print output

  let instructions2 = instructions & repeat & take 100000 & concat
  result2 <- doInstructions instructions2 starter
  print result2

-- "abcd" "bdca" -> [!! 1, !! 3, !! 2, !! 0]

doInstructions :: [Instruction s] -> V.Vector Char -> IO (V.Vector Char)
doInstructions instructions input = do
  buffer <- V.thaw input
  applyInstructions instructions buffer
  output <- V.freeze buffer
  pure output

applyInstructions :: [Instruction s] -> M.IOVector Char -> IO ()
applyInstructions instructions vector = mapM_ ($ vector) instructions

findIndexes :: String -> [Int]
findIndexes string
  = string
  & map ord
  & map (subtract (ord 'a'))

spin :: Int -> Instruction s
spin num string = do
  temp <- M.clone string
  M.copy (M.slice 0 num string) (M.slice (M.length temp - num) num temp)
  M.copy (M.slice num (M.length string - num) string) (M.slice 0 (M.length temp - num) temp)

exchange :: Int -> Int -> Instruction s
exchange index1 index2 string = M.swap string index1 index2

partner :: Char -> Char -> Instruction s
partner char1 char2 string = do
  v <- V.unsafeFreeze string
  let index1 = V.findIndex (== char1) v & fromJust
  let index2 = V.findIndex (== char2) v & fromJust
  exchange index1 index2 string

parseDance :: ReadP [Instruction s]
parseDance = sepBy parseInstruction (char ',')

parseInstruction :: ReadP (Instruction s)
parseInstruction = parseSpin <++ parseExchange <++ parsePartner

parseSpin :: ReadP (Instruction s)
parseSpin = do
  char 's'
  num <- parseInt
  pure $ spin num

parseExchange :: ReadP (Instruction s)
parseExchange = do
  char 'x'
  index1 <- parseInt
  char '/'
  index2 <- parseInt
  pure $ exchange index1 index2

parsePartner :: ReadP (Instruction s)
parsePartner = do
  char 'p'
  char1 <- get
  char '/'
  char2 <- get
  pure $ partner char1 char2
