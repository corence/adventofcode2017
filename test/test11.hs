
import Test.Hspec
import Test.QuickCheck
import Lib
import Hex

main :: IO ()
main = hspec $ do
  describe "Distance from origin" $ do
    it "understands a straight line" $ do
      let newPos = followPath origin [SE, SE, SE]
      distanceFromOrigin newPos `shouldBe` 3

    it "understands a southern zigzag" $ do
      let newPos = followPath origin [SE, SW, SE, SW]
      distanceFromOrigin newPos `shouldBe` 2

    it "can go from southeast to northeast" $ do
      let newPos = followPath origin [SE, SE, N, N]
      distanceFromOrigin newPos `shouldBe` 2

    it "understands a N/NW zigzag that doesn't end on a diagonal" $ do
      let newPos = followPath origin [NW, N, NW, NW]
      distanceFromOrigin newPos `shouldBe` 4

    it "understands a NW/NE zigzag that doesn't end on a diagonal" $ do
      let newPos = followPath origin [NW, NE, NW, NW]
      distanceFromOrigin newPos `shouldBe` 3

    it "cancels out opposite directions" $ do
      let newPos = followPath origin [N, N, N, S]
      distanceFromOrigin newPos `shouldBe` 2

    it "cancels out opposite directions indirectly" $ do
      let newPos = followPath origin [N, N, N, SW, SE, SW]
      distanceFromOrigin newPos `shouldBe` 2
