
import Day15Generator
import Test.Hspec
import Lib
import Text.Printf

stream1 = stream factorA 65
stream2 = stream factorB 8921

main :: IO ()
main = hspec $
  -- For example, suppose that for starting values, generator A uses 65, while generator B uses 8921.
  describe "sample: 65 and 8921" $ do
    it "a should generate the expected first 5 nums" $
      stream1 & take 5 `shouldBe` [1092455, 1181022009, 245556042, 1744312007, 1352636452]

    it "b should generate the expected first 5 nums" $
      stream2 & take 5 `shouldBe` [430625591, 1233683848, 1431495498, 137874439, 285222916]

    it "3rd value of both streams should match" $ do
      let va = stream1 !! 2
      let vb = stream2 !! 2
      putStrLn $ printf "%b" va
      putStrLn $ printf "%b" vb
      match (stream1 !! 2) (stream2 !! 2) `shouldBe` True

    it "should match 588 times out of 40 million" $
      countMatches stream1 stream2 `shouldBe` 588

{-
To get a significant sample, the judge would like to consider 40 million pairs.
(In the example above, the judge would eventually find a total of 588 pairs that match in their lowest 16 bits.)
-}
