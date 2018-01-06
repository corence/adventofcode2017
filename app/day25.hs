
import Lib
import Day25Turing
import qualified Data.Vector as V
import Control.Monad.ST

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
  let output1 = runST $ do
                  let program = actuallyParse parseProgram input1
                  executeProgram program
  print $ V.length output1

  -- input2 <- readFile "inputs/input25.txt"
  -- let output2 = runProgram input2
  -- print $ V.length output2
