
{-
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
-}

import qualified Data.Map as Map
import Data.Map(Map, (!))

import qualified Data.Set as Set
import Data.Set(Set)

import Text.ParserCombinators.ReadP
import Data.Char

(&) :: a -> (a -> b) -> b
(&) = flip ($)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

type Link = (Int, [Int])
type Links = Map Int [Int]
type Group = Set Int

word :: ReadP String
word = many1 (satisfy (not . isSpace))

parseLink :: ReadP Link
parseLink = do
  node <- word <&> read
  string " <-> "
  nodes <- sepBy (many1 (satisfy isDigit)) (string ", ") <&> map read
  eof
  pure $ (node, nodes)

-- choose one fully-parsed result, or crash if there's none
actuallyParse :: ReadP a -> String -> a
actuallyParse reader input
  = readP_to_S reader input & filter (null . snd) & head & fst

main :: IO ()
main = do
  links <- readFile "input12.txt" <&> lines <&> map (actuallyParse parseLink) <&> Map.fromList

  -- part 1
  let group = makeGroup links 0 Set.empty
  print $ Set.size group

  -- part 2
  let allNodes = links & Map.toList & map fst & Set.fromList
  let remainders = iterate (subtractAnyGroup links) allNodes
  let numGroups = takeWhile (not . Set.null) remainders & length
  print $ (Set.size allNodes)
  print $ numGroups

subtractAnyGroup :: Links -> Group -> Group
subtractAnyGroup links group = foldr Set.delete group (makeGroup links index Set.empty & Set.toList)
  where index = Set.toList group & head

makeGroup :: Links -> Int -> Group -> Group
makeGroup links node group = foldr consume group (links ! node)
  where consume node group
          = if Set.member node group
              then group
              else makeGroup links node (Set.insert node group)
