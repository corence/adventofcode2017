module Lib where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.List

(&) :: a -> (a -> b) -> b
(&) = flip ($)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

word :: ReadP String
word = many1 (satisfy (not . isSpace))

-- choose one fully-parsed result, or crash if there's none
actuallyParse :: ReadP a -> String -> a
actuallyParse reader input
  = let options = readP_to_S reader input & filter (null . snd)
    in if null options
          then error "whyyyy"
          else options & head & fst

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn extract lhs rhs
  = case compare (extract lhs) (extract rhs) of
           EQ -> lhs
           LT -> rhs
           GT -> lhs

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn extract lhs rhs
  = case compare (extract lhs) (extract rhs) of
           EQ -> rhs
           LT -> lhs
           GT -> rhs

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn extract = foldl1' (maxOn extract)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn extract = foldl1' (minOn extract)

combineComparators :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> (a -> a -> Ordering)
combineComparators comp1 comp2 a b
  = case comp1 a b of
      LT -> LT
      GT -> GT
      EQ -> comp2 a b

mergeComparators :: [a -> a -> Ordering] -> (a -> a -> Ordering)
mergeComparators [] = const $ const EQ -- no comparators means everything is equal
mergeComparators comparators = foldl1' combineComparators comparators

parseInt :: ReadP Int
parseInt = do
  sign <- option ' ' (char '-')
  digits <- many1 digit
  pure $ read (sign : digits)

digit :: ReadP Char
digit = satisfy isDigit
