
import Day22Grid
import Lib

main :: IO ()
main = do
  poses <- readFile "inputs/input22.txt" <&> inputToPoses
  let status = initState poses
  iterate update status & (!! 10000) & sInfectionEvents & length & print
  putStrLn "5353 is too low"

