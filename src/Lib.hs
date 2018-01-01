module Lib where

import Text.ParserCombinators.ReadP
import Data.Char

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
