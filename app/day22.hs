
import Day22Grid
import Lib
import Control.Monad

main :: IO ()
main = do
  let testStatus = [Pos 1 (-1), Pos (-1) 0] & initState
  realStatus <- readFile "inputs/input22.txt" <&> inputToPoses <&> initState
  iterate update realStatus & (!! 10000) & sInfectionEvents & length & print
  putStrLn "5353 is too low"

  iterate reconstitute testStatus & (!! 10000000) & sInfectionEvents & length & print
