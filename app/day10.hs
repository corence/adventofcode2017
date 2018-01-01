
import Lib
import Day10Rope
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed((!))
import Data.Char
import Data.Bits
import Text.Printf

rawLengths = "83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100"
lengths = [83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100]

main :: IO ()
main = do
  -- part 1
  let result = makeVector 256 & applyLengths lengths
  print $ result ! 0 * result ! 1

  -- part 2
  putStrLn $ knotHash rawLengths
