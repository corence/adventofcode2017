
import Test.Hspec
import Test.QuickCheck
import Day9Groups
import Debug.Trace
import Lib

parseAndCountGarbage :: String -> Int
parseAndCountGarbage input = actuallyParse parseEntry input & countGarbage

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

  describe "Count garbage" $ do
    it "<> is 0" $
      parseAndCountGarbage "<>" `shouldBe` 0
    it "<random characters> is 17" $
      parseAndCountGarbage "<random characters>" `shouldBe` 17
    it "<<<<> is 3" $
      parseAndCountGarbage "<<<<>" `shouldBe` 3
    it "<{!>}> is 2" $
      parseAndCountGarbage "<{!>}>" `shouldBe` 2
    it "<!!> is 0" $
      parseAndCountGarbage "<!!>" `shouldBe` 0
    it "<!!!>> is 0" $
      parseAndCountGarbage "<!!!>>" `shouldBe` 0
    it "<{o\"i!a,<{i<a> is 10" $
      parseAndCountGarbage "<{o\"i!a,<{i<a>" `shouldBe` 10
