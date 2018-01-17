
module Day22Grid where

import qualified Data.Map as Map
import Data.Map(Map, (!))
import Lib

data Pos = Pos Int Int deriving (Show, Eq, Ord)
data Dir = North | East | South | West deriving (Show, Eq)
data Health = Clean | Weakened | Infected | Flagged deriving (Show, Eq)

clockwise :: Dir -> Dir
clockwise North = East
clockwise East = South
clockwise South = West
clockwise West = North

counterclockwise :: Dir -> Dir
counterclockwise North = West
counterclockwise West = South
counterclockwise South = East
counterclockwise East = North

advance :: Dir -> Pos -> Pos
advance East  (Pos x y) = Pos (x + 1) y
advance North (Pos x y) = Pos x (y - 1)
advance West  (Pos x y) = Pos (x - 1) y
advance South (Pos x y) = Pos x (y + 1)

data Dude = Dude {
                 dudePos :: Pos,
                 dudeDirection :: Dir
                 } deriving (Show, Eq)

data Status = Status {
                sDude :: Dude,
                sInfections :: Map Pos Health,
                sInfectionEvents :: [Pos]
                } deriving Show

initState :: [Pos] -> Status
initState poses
  = Status (Dude (Pos 0 0) North) infections []
  where infections = zip poses (repeat Infected) & Map.fromList

update :: Status -> Status
update status@(Status dude infections infectionEvents)
  = (updateHealth newHealth pos . updateDude directionChange) status
  where pos = dudePos dude
        (newHealth, directionChange) = case infections ! pos of
                                        Clean -> (Infected, counterclockwise)
                                        Infected -> (Clean, clockwise)

-- this is the "update" function for part 2 -- with weakened and flagged nodes
reconstitute :: Status -> Status
reconstitute status@(Status dude infections infectionEvents)
  = (updateHealth newHealth pos . updateDude directionChange) status
  where pos = dudePos dude
        (newHealth, directionChange) = case infections ! pos of
                                        Clean -> (Weakened, counterclockwise)
                                        Weakened -> (Infected, id)
                                        Infected -> (Flagged, clockwise)
                                        Flagged -> (Clean, clockwise . clockwise)

updateHealth :: Health -> Pos -> Status -> Status
updateHealth health pos status
  = status { sInfections = Map.insert pos health (sInfections status) }

updateDude :: (Dir -> Dir) -> Status -> Status
updateDude directionChange status = status { sDude = Dude newPos newDirection }
  where newDirection = directionChange newDirection
        newPos = advance newDirection oldPos
        (Dude oldPos oldDirection) = sDude status

inputToPoses :: String -> [Pos]
inputToPoses input
  = zipWith (inscribeRow xMax yMax) [0..] values
  & concat
  where xMax = length (head inputs) `div` 2 -- assume every line has the same length
        yMax = length inputs `div` 2
        inputs = lines input
        values = map (map (== '#')) inputs

inscribeRow :: Int -> Int -> Int -> [Bool] -> [Pos]
inscribeRow xOffset yOffset y bools
  = zipWith (\x value -> (Pos (x - xOffset) (y - yOffset), value)) [0..] bools
  & filter snd
  & map fst
