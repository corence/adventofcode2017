
import Lib
import Day25Turing
import Day25TuringParser
import qualified Data.Vector as V
import Control.Monad.ST
import Text.Parsec(parse)
import Debug.Trace

main :: IO ()
main = do
  {-
  let output0 = runST $ do
                  let step0 = Step 0 []
                  let steps = V.fromList [step0, step1, step2]
                  let program = Program 1 4 steps
                  executeProgram program
  print $ V.length output0
  -}

  input1 <- readFile "inputs/input25test.txt"
  let input1a = input1 & lines & map (dropWhile (== ' ')) & unlines
  let output1 = runST $
                  case parse parseProgram "inputs/input25test.txt" input1a of
                    Left err -> pure $ Left err
                    Right program -> traceShow program $ executeProgram program <&> Right
  case output1 of
    Left err -> print err
    Right output -> print $ V.length output

  -- input2 <- readFile "inputs/input25.txt"
  -- let output2 = runProgram input2
  -- print $ V.length output2
