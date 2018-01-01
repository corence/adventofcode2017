
import Lib
import Test.Hspec
import Test.QuickCheck
import Day10Rope
import Data.Vector.Unboxed ((!))

main :: IO ()
main = hspec $ do
  describe "Acceptance tests" $ do
    it "is good" $ do
      let result = makeVector 5 & applyLengths [3, 4, 1, 5]
      putStrLn "waka waka"
      print result
      result ! 0 * result ! 1 `shouldBe` 12
