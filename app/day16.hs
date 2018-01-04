
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Lib
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import Control.Monad.ST

starter :: V.Vector Char
starter = V.fromList "abcdefghijklmnop"

type Instruction s = M.IOVector Char -> IO ()

main :: IO ()
main = do
  input <- readFile "inputs/input16.txt" <&> lines <&> head
  let instructions = actuallyParse parseDance input
  buffer <- V.thaw starter
  applyInstructions instructions buffer
  output <- V.freeze buffer
  print output
  let instructions2 = instructions & repeat & take 1000000 & concat
  result2 <- doInstructions instructions2 starter
  print result2
  putStrLn "a"

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
partner char1 char2 string
  = mapM_ doThing ([0..n] :: [Int])
  where n = M.length string - 1 :: Int
        doThing = M.modify string maybeSwap
        maybeSwap c
          | c == char1 = char2
          | c == char2 = char1
          | otherwise = c

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
