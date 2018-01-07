
module Day25Turing where

import Control.Monad.ST
import Control.Applicative
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Char
import Lib

newtype StepName = StepName Int deriving Show

nameToIndex :: StepName -> Int
nameToIndex (StepName index) = index

data Status s = Status
                  {
                    buffer :: MU.MVector s Int,
                    index :: STRef s Int,
                    nextStep :: STRef s StepName,
                    stepsTaken :: STRef s Int,
                    modifiedRange :: STRef s (Int, Int)
                  }

data Program s = Program
                 {
                   firstStep :: StepName,
                   numSteps :: Int,
                   steps :: V.Vector (Step s)
                 } deriving (Show)

data Step s = Step
              {
                name :: StepName,
                falseFunction :: Function s,
                trueFunction :: Function s
              }

instance Show (Step s) where
  show (Step name false true) =
        "(Step "
        ++ show name
        ++ ", "
        ++ show (length false)
        ++ ", "
        ++ show (length true)
        ++ ")"

type Function s = [Instruction s]
type Instruction s = Status s -> ST s ()

finalBuffer :: Status s -> ST s (VU.Vector Int)
finalBuffer status = do
  frozen <- VU.freeze (buffer status)
  (minIndex, maxIndex) <- readSTRef (modifiedRange status)
  pure $ VU.slice minIndex (maxIndex + 1 - minIndex) frozen

initialStatus :: Program s -> ST s (Status s)
initialStatus program
  =   Status
  <$> MU.new 10000000
  <*> newSTRef 5000000
  <*> newSTRef (firstStep program)
  <*> newSTRef 0
  <*> newSTRef (5000000, 5000000)

advanceProgram :: Program s -> Status s -> ST s ()
advanceProgram program status = do
  indexVal <- readSTRef (index status)
  stepName <- readSTRef (nextStep status)
  stepsSoFar <- readSTRef (stepsTaken status)
  let step = steps program V.! nameToIndex stepName
  if numSteps program - stepsSoFar > 0
    then executeStep step status >> advanceProgram program status
    else pure ()

executeStep :: Step s -> Status s -> ST s ()
executeStep step status = do
  modifySTRef (stepsTaken status) (+ 1)
  currentIndex <- index status & readSTRef
  currentValue <- MU.read (buffer status) currentIndex
  if currentValue > 0
    then executeFunction (trueFunction  step) status
    else executeFunction (falseFunction step) status

executeFunction :: Function s -> Status s -> ST s ()
executeFunction function status = mapMonads status function

mapMonads :: Monad m => a -> [a -> m ()] -> m ()
mapMonads a list = map ($ a) list & sequence_

executeProgram :: Program s -> ST s (VU.Vector Int)
executeProgram program = do
  status <- initialStatus program
  advanceProgram program status
  finalBuffer status

instructionWrite :: Int -> Instruction s
instructionWrite value status = do
  writeIndex <- readSTRef (index status)
  MU.write (buffer status) writeIndex value
  modifySTRef (modifiedRange status) (extendRange writeIndex)
    where extendRange n (low, high) = (min n low, max n high)

instructionMove :: (Int -> Int) -> Instruction s
instructionMove modifier status
  = modifySTRef (index status) modifier

instructionNextStep :: StepName -> Instruction s
instructionNextStep name status
  = writeSTRef (nextStep status) name
