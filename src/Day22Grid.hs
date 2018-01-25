
module Day22Grid where

import qualified Data.Map as Map
import Data.Map(Map)
import Lib
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST
import Data.STRef

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

data Dude s = Dude {
                 dudePos :: STRef s Pos,
                 dudeDirection :: STRef s Dir
                 } deriving (Eq)

data Status s = Status {
                sGridXMax :: Int,
                sGridYMax :: Int,
                sDude :: Dude s,
                sInfections :: VM.MVector s Health,
                sNumInfectionEvents :: STRef s Int
                }

instance Show (Status s) where
  show (Status xMax yMax dude infections numInfectionEvents)
    = "omg its a status"

initState :: Int -> Int -> [Pos] -> ST s (Status s)
initState xMax yMax poses
  =   Status xMax yMax
  <$> (Dude <$> newSTRef (Pos 0 0) <*> newSTRef North)
  <*> VM.replicate gridSize Clean
  <*> newSTRef 0
  where infections = zip poses (repeat Infected) & Map.fromList
        gridSize = (xMax * 2 + 1) * (yMax * 2 + 1)

posToIndex :: Status s -> Pos -> Int
posToIndex status@(Status xMax yMax _ _ numInfectionEvents) pos@(Pos px py)
  = y * width + x
  where y = py + yMax
        x = px + xMax
        width = xMax * 2 + 1

update :: Status s -> ST s ()
update status@(Status _ _ dude infections infectionEvents)
  = do
  pos <- readSTRef (dudePos dude)
  oldHealth <- VM.read infections (posToIndex status pos)
  let (newHealth, directionChange) = case oldHealth of
                                        Clean -> (Infected, counterclockwise)
                                        Infected -> (Clean, clockwise)
  updateHealth newHealth pos status
  updateDude directionChange status

-- this is the "update" function for part 2 -- with weakened and flagged nodes
reconstitute :: Status s -> ST s ()
reconstitute status@(Status _ _ dude infections infectionEvents)
  = do
    pos <- readSTRef $ dudePos dude
    oldHealth <- VM.read infections (posToIndex status pos)
    let (newHealth, directionChange)
          = case oldHealth of
              Clean -> (Weakened, counterclockwise)
              Weakened -> (Infected, id)
              Infected -> (Flagged, clockwise)
              Flagged -> (Clean, clockwise . clockwise)
    updateHealth newHealth pos status
    updateDude directionChange status

updateHealth :: Health -> Pos -> Status s -> ST s ()
updateHealth health pos status@(Status _ _ _ sInfections sNumInfectionEvents) = do
  VM.write sInfections ( posToIndex status pos) health
  if health == Infected
      then modifySTRef sNumInfectionEvents (+ 1)
      else pure ()

updateDude :: (Dir -> Dir) -> Status s -> ST s ()
updateDude directionChange status = do
  oldPos <- readSTRef $ dudePos (sDude status)
  oldDirection <- readSTRef $ dudeDirection (sDude status)
  let newDirection = directionChange oldDirection
  let newPos = advance newDirection oldPos
  writeSTRef (dudePos (sDude status)) newPos
  writeSTRef (dudeDirection (sDude status)) newDirection

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
