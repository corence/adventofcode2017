
import Lib
import Day25Turing
import Day25TuringParser
import qualified Data.Vector.Unboxed as VU
import Control.Monad.ST
import Text.Parsec(parse, ParseError)
import Debug.Trace

main :: IO ()
main = do
  input1 <- readFile "inputs/input25test.txt"
  case parseAndExecute "inputs/input25test.txt" input1 of
    Left err -> print err
    Right output -> if VU.length output > 0
                      then do
                        putStrLn "---"
                        print $ VU.length $ VU.filter (> 0) output
                        putStrLn "###"
                        print output
                      else putStrLn "what!!! the!!! dam!!!"

  putStrLn "part 1!"
  input2 <- readFile "inputs/input25.txt"
  case parseAndExecute "inputs/input25test.txt" input2 of
    Left err -> print err
    Right output -> if VU.length output > 0
                      then do
                        putStrLn "---"
                        print $ VU.length $ VU.filter (> 0) output
                        putStrLn "###"
                        print output
                      else putStrLn "what!!! the!!! dam!!!"



parseAndExecute :: String -> String -> Either ParseError (VU.Vector Int)
parseAndExecute filename fileContents
  = runST $ do
      let input = fileContents & lines & map (dropWhile (== ' ')) & unlines
      case parse parseProgram filename input of
        Left err -> pure $ Left err
        Right program -> traceShow program $ executeProgram program <&> Right
