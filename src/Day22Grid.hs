
module Day22Grid where

import qualified Data.Set as Set
import Data.Set(Set)

data Pos = Pos Int Int deriving (Show, Eq, Ord)
data Dir = North | East | South | West

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
advance North (Pos x y) = Pos (x + 1) y
advance East  (Pos x y) = Pos x (y + 1)
advance South (Pos x y) = Pos (x - 1) y
advance West  (Pos x y) = Pos x (y - 1)

data Action = Action {
                oldPos :: Pos,
                newDirection :: Dir,
                infect :: Bool
                }

data State = State {
                dudePos :: Pos,
                dudeDirection :: Dir,
                infections :: Set Pos
                }

nextAction :: State -> Action
nextAction (State dudePos dudeDirection infections)
  = Action dudePos newDirection infect
    where infect = Set.member dudePos infections
          newDirection = if infect then clockwise dudeDirection else counterclockwise dudeDirection

applyAction :: Action -> State -> State
applyAction (Action oldPos newDirection infect) (State dudePos dudeDirection infections)
  = State newPos newDirection newInfections
    where newPos = advance newDirection oldPos
          newInfections = if infect then Set.insert oldPos infections else Set.delete oldPos infections

initState :: [Pos] -> State
initState poses = State (Pos 0 0) North (Set.fromList poses)
