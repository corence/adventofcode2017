
{-# LANGUAGE TupleSections #-}

module Day21PatternMatcher where

import Day21BoolStreams
import Lib
import qualified Data.Map as Map
import Data.Map(Map)
import Data.List(sort)

data Pattern = Pattern { pRowLength :: Int, pPixels :: [Bool] }
type Matchers = Map [Bool] [Bool]

instance Show Pattern where
  show (Pattern rowLength pixels) = "Pattern " ++ show rowLength ++ " " ++ showGrid pixels

toPattern :: String -> Pattern
toPattern string = Pattern rowLength pixels
  where pixels = (map (== '#') . filter (/= '/')) string
        rowLength = ((+ 1) . length . filter (== '/')) string

initialPattern :: Pattern
initialPattern = ".#./..#/###" & toPattern

testMatchers :: Matchers
testMatchers
  = ["../.# => ##./#../...",
     ".#./..#/### => #..#/..../..../#..#"]
  & map parseMatchers
  & foldr1 Map.union

load :: IO Matchers
load = do
  input <- readFile "inputs/input21.txt"
  let inputs = lines input
  let matchers = foldr (\input -> Map.union (parseMatchers input)) Map.empty inputs
  pure matchers

parseMatchers :: String -> Map [Bool] [Bool]
parseMatchers inputLine
  = let (Pattern rowLength source, Pattern _ dest) = parseMatch inputLine
  in permeate rowLength source
  & map (, dest)
  & Map.fromList

parseMatch :: String -> (Pattern, Pattern)
parseMatch input = (source, dest)
  where source = toPattern (words input !! 0)
        dest = toPattern (words input !! 2)

enhance :: Matchers -> Pattern -> Pattern
enhance matchers (Pattern rowLength input)
  | rowLength `mod` 2 == 0 = enhanceBy 2
  | rowLength `mod` 3 == 0 = enhanceBy 3
  | otherwise = error $ "panic, someone just called enhance with rowLength=" ++ show rowLength
  where enhanceBy subLength
          = input
          & toSubgrids rowLength subLength
          & map (matchers Map.!)
          & fromSubgrids rowLength 2
          & Pattern (rowLength + numRows)
          where numRows = rowLength `div` subLength

showGrid :: [Bool] -> String
showGrid = map (\value -> if value then '#' else '.')

drawPattern :: Pattern -> String
drawPattern (Pattern rowLength pixels)
  = pixels
  & toRows rowLength
  & map showGrid
  & unlines
