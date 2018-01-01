
import Day9Groups
import Lib

main :: IO ()
main = do
   entry <- readFile "inputs/input9.txt" <&> actuallyParse parseEntry
   print $ scoreGroups 1 entry
   print $ countGarbage entry
