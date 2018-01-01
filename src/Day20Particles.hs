
module Day20Particles where

import Data.List
import Lib
import qualified Data.Map.Strict as Map
import Data.Map(Map)
import Data.Bifunctor

data Vec3 = Vec3 {
            x :: Int,
            y :: Int,
            z :: Int
            } deriving (Show, Ord, Eq)

origin = Vec3 0 0 0

data Particle = Particle {
                  pid :: Int,
                  pos :: Vec3,
                  vel :: Vec3,
                  acc :: Vec3
                  } deriving (Show)

data Cube = Cube {
              cMin :: Vec3,
              cMax :: Vec3
              }

data Limits = Limits {
              lPos :: Cube,
              lVel :: Cube,
              lAcc :: Cube
              }

magnitude :: Vec3 -> Int
magnitude (Vec3 x y z) = abs x + abs y + abs z

originest :: [Particle] -> Particle
originest = minimumBy longerTraveller

longerTraveller :: Particle -> Particle -> Ordering
longerTraveller = ordFuncs & map ordFuncToComparator & mergeComparators
  where ordFuncs = [
                      magnitude . acc,
                      magnitude . vel,
                      magnitude . pos,
                      pid
                      ]

ordFuncToComparator :: Ord o => (a -> o) -> a -> a -> Ordering
ordFuncToComparator func a b = compare (func a) (func b)

findLimits :: [Particle] -> Limits
findLimits particles
  = Limits
      (findLimit pos particles)
      (findLimit vel particles)
      (findLimit acc particles)

findLimit :: (Particle -> Vec3) -> [Particle] -> Cube
findLimit func = foldr stretchLimits (Cube origin origin)
  where stretchLimits particle = stretchCube (func particle)

stretchCube :: Vec3 -> Cube -> Cube
stretchCube addition (Cube cMin cMax) = Cube (vecMix min cMin addition) (vecMix max cMax addition)

vecMix :: (Int -> Int -> Int) -> Vec3 -> Vec3 -> Vec3
vecMix func (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (func x1 x2) (func y1 y2) (func z1 z2)

isOutlier :: Limits -> Particle -> Bool
isOutlier limits particle
  = checks & map ($ limits) & map ($ particle) & or
  where checks = [
          isOutlierVia cMax x,
          isOutlierVia cMax y,
          isOutlierVia cMax z,
          isOutlierVia cMax x,
          isOutlierVia cMin y,
          isOutlierVia cMin z
          ]

isOutlierVia :: (Cube -> Vec3) -> (Vec3 -> Int) -> Limits -> Particle -> Bool
isOutlierVia decube dimensionize limits particle
  = (dimensionize $ pos particle) == (dimensionize $ decube $ lPos limits)
  && (dimensionize $ vel particle) == (dimensionize $ decube $ lVel limits)
  && (dimensionize $ acc particle) == (dimensionize $ decube $ lAcc limits)


-- eliminate collisions:
-- 1) put all particles into a Map Vec3 [Particle]
-- 2) (snd . Map.toList)
-- 3) filter (== 1 . length)
-- 4) concat

eliminateCollisions :: [Particle] -> [Particle]
eliminateCollisions particles
  = particles
  & foldr addParticleByPosition Map.empty
  & Map.toList
  & map snd
  & filter ((== 1) . length)
  & concat
  where addParticleByPosition particle
          = Map.insertWith (++) (pos particle) [particle]

-- eliminate and count outliers:
-- 1) calculate the limits
-- 2) partition via isOutlier
-- 3) count the outliers and return the others

-- returns: (outliers, not-outliers)
separateOutliers :: [Particle] -> ([Particle], [Particle])
separateOutliers particles
  = particles
  & partition (isOutlier (findLimits particles))

update :: Particle -> Particle
update (Particle pid pos vel acc)
  = Particle pid newPos newVel acc
    where newPos = vecAdd pos newVel
          newVel = vecAdd vel acc
          vecAdd = vecMix (+)

calculateOutliers :: [Particle] -> [Particle]
calculateOutliers [] = []
calculateOutliers particles
  = particles
  & eliminateCollisions
  & separateOutliers
  & second (map update)
  & second calculateOutliers
  & uncurry (++)


-- solving parallel shots, and converging shots:
-- parallel: (same pos, same vel, same acc) (diff pos, same vel, same acc)
-- converging: (same pos, same vel, same acc) (diff pos, diff vel, diff acc)
-- 1) i have pulled _ahead_ of the pack in pos, vel, and acc
-- 2) my x, y, or z is "diverging"
-- 3) my x, y, and z are in "stasis"
