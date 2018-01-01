
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
    let update (actions, state)
         = let action = nextAction state
           in (action, applyAction action state)
    let actionStream = iterate update (Action (Pos 0 0) North False, initState input)
    it "given example 1" $ do
      let infections = take 7 actionStream & filter (infect . fst)
      length infections `shouldBe` 5
    it "given example 2" $ do
      let infections = take 70 actionStream & filter (infect . fst)
      length infections `shouldBe` 41
    it "given example 3" $ do
      let infections = take 10000 actionStream & filter (infect . fst)
      length infections `shouldBe` 5587
