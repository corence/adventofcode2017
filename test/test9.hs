
import Test.Hspec
import Test.QuickCheck
import Day9Groups
import Debug.Trace

main :: IO ()
main = hspec $ do
  describe "Group count" $ do
    it "{} is 1" $
      parseCountGroups "{}" `shouldBe` 1
    it "{{{}}} is 3" $
      parseCountGroups "{{{}}}" `shouldBe` 3
    it "{{{},{},{{}}}} is 6" $
      parseCountGroups "{{{},{},{{}}}}" `shouldBe` 6
    it "{<{},{},{{}}>} is 1" $
      parseCountGroups "{<{},{},{{}}>}" `shouldBe` 1
    it "{<a>,<a>,<a>,<a>} is 1" $
      parseCountGroups "{<a>,<a>,<a>,<a>}" `shouldBe` 1
    it "{{<a>},{<a>},{<a>},{<a>}} is 5" $
      parseCountGroups "{{<a>},{<a>},{<a>},{<a>}}" `shouldBe` 5
    it "{{<!>},{<!>},{<!>},{<a>}} is 2" $
      parseCountGroups "{{<!>},{<!>},{<!>},{<a>}}" `shouldBe` 2

  describe "Score" $ do
    it "{} is 1" $
      score "{}" `shouldBe` 1
    it "{{{}}} is 6" $
      score "{{{}}}" `shouldBe` 6
    it "{{},{}} is 5" $
      score "{{},{}}" `shouldBe` 5
    it "{{{},{},{{}}}} is 16" $
      score "{{{},{},{{}}}}" `shouldBe` 16
    it "{<a>,<a>,<a>,<a>} is 1" $
      score "{<a>,<a>,<a>,<a>}" `shouldBe` 1
    it "{{<ab>},{<ab>},{<ab>},{<ab>}} is 9" $
      score "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 9
    it "{{<!!>},{<!!>},{<!!>},{<!!>}} is 9" $
      score "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` 9
    it "{{<a!>},{<a!>},{<a!>},{<ab>}} is 3" $
      score "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` 3
