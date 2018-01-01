
import Lib
import Test.Hspec
import Test.QuickCheck
import Day20Particles

main :: IO ()
main = hspec $
  describe "Staying near the origin" $
    it "the given example" $ do
      let particles = [
                        Particle 0 (Vec3 3 0 0) (Vec3 2 0 0) (Vec3 (-1) 0 0),
                        Particle 1 (Vec3 4 0 0) (Vec3 0 0 0) (Vec3 (-2) 0 0)
                        ]
      (originest particles & pid) `shouldBe` 0
