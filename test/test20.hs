
import Lib
import Test.Hspec
import Test.QuickCheck
import Day20Particles

main :: IO ()
main = hspec $ do
  describe "Staying near the origin" $
    it "the given example" $ do
      let particles = [
                        Particle 0 (Vec3 3 0 0) (Vec3 2 0 0) (Vec3 (-1) 0 0),
                        Particle 1 (Vec3 4 0 0) (Vec3 0 0 0) (Vec3 (-2) 0 0)
                        ]
      (originest particles & pid) `shouldBe` 0
  describe "Counting outliers" $ do
    it "the given example" $ do
      let particles = [
                        Particle 0 (Vec3 (-6) 0 0) (Vec3 3 0 0) (Vec3 0 0 0),
                        Particle 1 (Vec3 (-4) 0 0) (Vec3 2 0 0) (Vec3 0 0 0),
                        Particle 2 (Vec3 (-2) 0 0) (Vec3 1 0 0) (Vec3 0 0 0),
                        Particle 3 (Vec3 3 0 0) (Vec3 (-1) 0 0) (Vec3 0 0 0)
                        ]
      (calculateDivergers particles & length) `shouldBe` 1
    it "parallel shots" $ do
      let particles = [
                        Particle 0 (Vec3 3 0 0) (Vec3 0 2 0) (Vec3 0 0 1),
                        Particle 1 (Vec3 6 0 0) (Vec3 0 2 0) (Vec3 0 0 1)
                        ]
      (calculateDivergers particles & length) `shouldBe` 2
    it "trailing shot" $ do
      let particles = [
                        Particle 0 (Vec3 6 3 0) (Vec3 0 2 0) (Vec3 0 0 0),
                        Particle 1 (Vec3 6 0 0) (Vec3 0 2 0) (Vec3 0 0 0)
                        ]
      (calculateDivergers particles & length) `shouldBe` 2
    it "converging shots" $ do
      let particles = [
                        Particle 0 (Vec3 0 0 0) (Vec3 1 2 0) (Vec3 0 0 0),
                        Particle 1 (Vec3 6 0 0) (Vec3 (-1) 2 0) (Vec3 0 0 0)
                        ]
      (calculateDivergers particles & length) `shouldBe` 0
    it "static placements" $ do
      let particles = [
                        Particle 0 (Vec3 0 6 0) (Vec3 0 0 0) (Vec3 0 0 0),
                        Particle 1 (Vec3 6 0 0) (Vec3 0 0 0) (Vec3 0 0 0)
                        ]
      (calculateDivergers particles & length) `shouldBe` 2
    it "more static placements" $ do
      let particles = [
                        Particle 0 (Vec3 0 6 0) (Vec3 0 0 0) (Vec3 0 0 0),
                        Particle 1 (Vec3 6 0 0) (Vec3 0 0 0) (Vec3 0 0 0),
                        Particle 2 (Vec3 6 6 0) (Vec3 0 0 0) (Vec3 0 0 0),
                        Particle 3 (Vec3 0 0 0) (Vec3 0 0 0) (Vec3 0 0 0)
                        ]
      (calculateDivergers particles & length) `shouldBe` 4
    it "catching up due to higher acceleration" $ do
      let particles = [
                        Particle 0 (Vec3 0 0 0) (Vec3 (-2) 0 0) (Vec3 2 0 0),
                        Particle 1 (Vec3 3 0 0) (Vec3 1 0 0) (Vec3 1 0 0)
                        ]
                        -- (0,-2,2) (3,1,1)
                        -- (0,0,2) (5,2,1)
                        -- (2,2,2) (8,3,1)
                        -- (6,4,2) (12,4,1)
                        -- (12,6,2) (17,5,1)
                        -- (20,8,2) (23,6,1)
                        -- (30,10,2) (30,7,1)
      (calculateDivergers particles & length) `shouldBe` 0
    it "diverge, then converge later" $ do
      let particles = [
                        Particle 0 (Vec3 0 0 0) (Vec3 6 1 0) (Vec3 (-2) 0 0),
                        Particle 1 (Vec3 0 10 0) (Vec3 (-9) (-1) 0) (Vec3 3 0 0)
                        ]
                        -- (0,0,0) (0,10,0)
                        -- (4,1,0) (-6,9,0)
                        -- (6,2,0) (-9,8,0)
                        -- (6,3,0) (-9,7,0)
                        -- (4,4,0) (-6,6,0)
                        -- (0,5,0) (0,5,0)
      (calculateDivergers particles & length) `shouldBe` 0
