
module Day22Grid where

import qualified Data.Set as Set
import Data.Set(Set)
import Lib

data Pos = Pos Int Int deriving (Show, Eq, Ord)
data Dir = North | East | South | West deriving Show

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
advance North (Pos x y) = Pos x (y + 1)
advance West  (Pos x y) = Pos (x - 1) y
advance South (Pos x y) = Pos x (y - 1)

data Dude = Dude {
                 dudePos :: Pos,
                 dudeDirection :: Dir
                 } deriving Show

data Status = Status {
                dude :: Dude,
                sInfections :: Set Pos,
                sInfectionEvents :: [Pos]
                } deriving Show

initState :: [Pos] -> Status
initState poses = Status (Dude (Pos 0 0) North) (Set.fromList poses) []

update :: Status -> Status
update status@(Status dude infections infectionEvents)
  = let pos = dudePos dude
    in if Set.member pos infections
      then clearInfection pos status & updateDude False
      else createInfection pos status & updateDude True

clearInfection :: Pos -> Status -> Status
clearInfection pos status = status { sInfections = Set.delete pos (sInfections status) }

createInfection :: Pos -> Status -> Status
createInfection pos status = status { sInfections = Set.insert pos (sInfections status), sInfectionEvents = pos : sInfectionEvents status }

updateDude :: Bool -> Status -> Status
updateDude isInfecting status = status { dude = Dude newPos newDirection }
  where newDirection = if isInfecting then counterclockwise oldDirection else clockwise oldDirection
        newPos = advance newDirection oldPos
        (Dude oldPos oldDirection) = dude status
