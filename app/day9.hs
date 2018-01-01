
import Day9Groups
import Lib

main :: IO ()
main = readFile "inputs/input9.txt" <&> score >>= print
