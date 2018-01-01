
import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Acceptance tests" $ do
    it "is good" $ do
        1 `shouldBe` 1
