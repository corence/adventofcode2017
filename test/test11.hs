
import Test.Hspec
import Test.QuickCheck
import Lib
import Hex
import Debug.Trace

import qualified Data.Map as Map
import Data.Map(Map)

expectValue :: Dir -> Map Dir Int -> Int -> ()
expectValue = undefined

main :: IO ()
main = hspec $ do
  describe "Cancel" $ do
    it "is a no-op on a straight line" $ do
      let newPos = followPath origin [S, S, S]
      let cancelled = cancel N S newPos
      cancel N S newPos `shouldBe` newPos
      Map.lookup SE cancelled `shouldBe` Nothing
      Map.lookup SW cancelled `shouldBe` Nothing
      Map.lookup  N cancelled `shouldBe` Nothing
      Map.lookup  S cancelled `shouldBe` Just 3

    it "is a no-op when it misses the line" $ do
      let newPos = followPath origin [SE, SE, SE]
      cancel N S newPos `shouldBe` newPos

  describe "Transmute" $ do
    it "converts big numbers to a zero and a nonzero" $ do
      let newPos = followPath origin [S, SE, SE, SW, SW, SW, SE, SW]
      let transmuted = transmute S SE SW newPos
      Map.lookup SE transmuted `shouldBe` Just 0
      Map.lookup SW transmuted `shouldBe` Just 1
      Map.lookup  S transmuted `shouldBe` Just 4

    it "is a no-op if a source is zero" $ do
      let newPos = followPath origin [SE, SE, SE]
      let transmuted = transmute S SE SW newPos
      Map.lookup SE transmuted `shouldBe` Just 3
      Map.lookup SW transmuted `shouldBe` Nothing
      Map.lookup  S transmuted `shouldBe` Just 0

  describe "Normalize" $ do
    it "is a no-op on a straight line" $ do
      let newPos = followPath origin [SE, SE, SE]
      let normalized = normalize newPos
      Map.lookup SE normalized `shouldBe` Just 3
      Map.lookup SW normalized `shouldBe` Just 0
      Map.lookup  S normalized `shouldBe` Just 0
      Map.lookup NW normalized `shouldBe` Just 0
      Map.lookup NE normalized `shouldBe` Just 0
      Map.lookup  N normalized `shouldBe` Just 0

    it "cancels all directions" $ do
      let newPos = followPath origin [N, NE, NW, SE, SW, S]
      let normalized = normalize newPos
      Map.lookup SE normalized `shouldBe` Just 0
      Map.lookup SW normalized `shouldBe` Just 0
      Map.lookup  S normalized `shouldBe` Just 0
      Map.lookup NW normalized `shouldBe` Just 0
      Map.lookup NE normalized `shouldBe` Just 0
      Map.lookup  N normalized `shouldBe` Just 0

  describe "Distance from origin" $ do
    it "understands a straight line" $ do
      let newPos = followPath origin [SE, SE, SE]
      trace (show newPos) $ distanceFromOrigin newPos `shouldBe` 3

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

    it "satisfies the samples given in the question" $ do
      runTests distanceFromOrigin [
        ([NE, NE, NE], 3),
        ([NE, NE, SW, SW], 0),
        ([NE, NE, S, S], 2),
        ([SE, SW, SE, SW, SW], 3)
        ]

runTests :: (Eq a, Show a) => (Pos -> a) -> [([Dir], a)] -> IO ()
runTests func datums = mapM_ checkEntry datums
  where checkEntry (dirs, expected) = func (followPath origin dirs) `shouldBe` expected
