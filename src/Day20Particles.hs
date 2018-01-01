
module Day20Particles where

import Data.List
import Lib

data Vec3 = Vec3 {
            x :: Int,
            y :: Int,
            z :: Int
            } deriving (Show)

data Particle = Particle {
                  pid :: Int,
                  pos :: Vec3,
                  vel :: Vec3,
                  acc :: Vec3
                  } deriving (Show)

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
