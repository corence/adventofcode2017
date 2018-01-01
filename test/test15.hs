
import Day15Generator
import Test.Hspec
import Lib

main :: IO ()
main = hspec $
  -- For example, suppose that for starting values, generator A uses 65, while generator B uses 8921.
  describe "sample: 65 and 8921" $ do
    it "a should generate the expected first 5 nums" $
      iterate (step factorA) 65 & take 5 `shouldBe` [1092455, 1181022009, 245556042, 1744312007, 1352636452]

    it "b should generate the expected first 5 nums" $
      iterate (step factorB) 8921 & take 5 `shouldBe` [430625591, 1233683848, 1431495498, 137874439, 285222916]

{-
In binary, these pairs are (with generator A's value first in each pair):

00000000000100001010101101100111
00011001101010101101001100110111

01000110011001001111011100111001
01001001100010001000010110001000

00001110101000101110001101001010
01010101010100101110001101001010

01100111111110000001011011000111
00001000001101111100110000000111

01010000100111111001100000100100
00010001000000000010100000000100

Here, you can see that the lowest (here, rightmost) 16 bits of the third value match: 1110001101001010. Because of this one match, after processing these five pairs, the judge would have added only 1 to its total.

To get a significant sample, the judge would like to consider 40 million pairs. (In the example above, the judge would eventually find a total of 588 pairs that match in their lowest 16 bits.)
-}
