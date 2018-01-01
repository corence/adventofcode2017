
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

vecMix :: (Int -> Int -> Int) -> Vec3 -> Vec3 -> Vec3
vecMix func (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (func x1 x2) (func y1 y2) (func z1 z2)

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

update :: Particle -> Particle
update (Particle pid pos vel acc)
  = Particle pid newPos newVel acc
    where newPos = vecAdd pos newVel
          newVel = vecAdd vel acc
          vecAdd = vecMix (+)

calculateDivergers :: [Particle] -> [Particle]
calculateDivergers [] = []
calculateDivergers particles
  = particles
  & eliminateCollisions
  & separateDivergers
  & second (map update)
  & second calculateDivergers
  & uncurry (++)

separateDivergers :: [Particle] -> ([Particle], [Particle])
separateDivergers particles
  = partition (mustDiverge particles) particles

mustDiverge :: [Particle] -> Particle -> Bool
mustDiverge particles = not . canConverge particles

canConverge :: [Particle] -> Particle -> Bool
canConverge particles particle = any (canConvergeWith particle) particles
  where canConvergeWith p1 p2 = canConvergeInDimension x p1 p2
                             && canConvergeInDimension y p1 p2
                             && canConvergeInDimension y p1 p2

-- solving parallel shots, and converging shots:
-- parallel: (same pos, same vel, same acc) (diff pos, same vel, same acc)
-- converging: (same pos, same vel, same acc) (diff pos, diff vel, diff acc)
-- 1) i have pulled _ahead_ of the pack in pos, vel, and acc
-- 2) my x, y, or z is "diverging"
-- 3) my x, y, and z are in "stasis"

-- consider this. Two points might converge IF between their pos, vel and acc there's one LT and one GT

canConvergeInDimension :: (Vec3 -> Int) -> Particle -> Particle -> Bool
canConvergeInDimension dimensionize p1 p2
  =  (elem LT comparisons && elem GT comparisons)
  || (pid p1 /= pid p2 && all (== EQ) comparisons)
  where comparisons = [
          compare (pos p1 & dimensionize) (pos p2 & dimensionize),
          compare (vel p1 & dimensionize) (vel p2 & dimensionize),
          compare (acc p1 & dimensionize) (acc p2 & dimensionize)
          ]

{-
 pos vel acc can_converge?
  <   <   <  false
  <   <   =  false
  <   <   >  true
  <   =   <  false
  <   =   =  false
  <   =   >  true
  <   >   <  true
  <   >   =  true
  <   >   >  true
  =   <   <  false
  =   <   =  false
  =   <   >  true
  =   =   <  false
  =   =   =  false
  =   =   >  false
  =   >   <  true
  =   >   =  false
  =   >   >  false
  >   <   <  true
  >   <   =  true
  >   <   >  true
  >   =   <  true
  >   =   =  false
  >   =   >  false
  >   >   <  true
  >   >   =  false
  >   >   >  false
-}

{-
 pos vel acc can_converge?
  <   <   >  true
  <   =   >  true
  <   >   <  true
  <   >   =  true
  <   >   >  true
  =   <   >  true
  =   >   <  true
  >   <   <  true
  >   <   =  true
  >   <   >  true
  >   =   <  true
  >   >   <  true
-}

{-
  <   <   <  false
  <   <   =  false
  <   =   <  false
  <   =   =  false
  =   <   <  false
  =   <   =  false
  =   =   <  false
  =   =   =  false
  =   =   >  false
  =   >   =  false
  =   >   >  false
  >   =   =  false
  >   =   >  false
  >   >   =  false
  >   >   >  false
-}

-- so how can we scan this without using quadratic time to check it?
