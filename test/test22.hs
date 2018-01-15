
import Lib
import Test.Hspec
import Test.QuickCheck
import Day22Grid
import Data.Bifunctor

import qualified Data.Set as Set
import Data.Set(Set)

main :: IO ()
main = hspec $ do
  describe "Part 1" $ do
    let input = [Pos 1 1, Pos (-1) 0]
    let actionStream = iterate update (initState input)
    it "given example 1" $ do
      let infections = actionStream !! 7 & sInfectionEvents
      length infections `shouldBe` 5
    it "given example 2" $ do
      let infections = actionStream !! 70 & sInfectionEvents
      length infections `shouldBe` 41
    it "given example 3" $ do
      let infections = actionStream !! 10000 & sInfectionEvents
      length infections `shouldBe` 5587
