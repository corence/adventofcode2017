
module Day25Turing where

import Control.Monad.ST
import Control.Applicative
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Char
import Lib

data Status s = Status
                  {
                    buffer :: M.MVector s Int,
                    index :: STRef s Int,
                    nextStep :: STRef s Int
                  }

data Program s = Program
                 {
                   firstStep :: Int,
                   numSteps :: Int,
                   steps :: V.Vector (Step s)
                 } deriving (Show)

data Step s = Step
              {
                name :: Int,
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

finalBuffer :: Status s -> ST s (V.Vector Int)
finalBuffer status = undefined

initialStatus :: Program s -> ST s (Status s)
initialStatus program
  = liftA3 Status (M.new 10000000) (newSTRef 5000000) (newSTRef $ firstStep program)

advanceProgram :: Program s -> Status s -> ST s ()
advanceProgram program status = do
  indexVal <- readSTRef (index status)
  stepName <- readSTRef (nextStep status)
  let step = steps program V.! stepName
  if numSteps program > 0
    then executeStep step status >> advanceProgram program status
    else pure ()

executeStep :: Step s -> Status s -> ST s ()
executeStep step status = do
  currentIndex <- index status & readSTRef
  currentValue <- M.read (buffer status) currentIndex
  if currentValue > 0
    then executeFunction (trueFunction  step) status
    else executeFunction (falseFunction step) status

executeFunction :: Function s -> Status s -> ST s ()
executeFunction function status = mapMonads status function

mapMonads :: Monad m => a -> [a -> m ()] -> m ()
mapMonads a list = map ($ a) list & sequence_

executeProgram :: Program s -> ST s (V.Vector Int)
executeProgram program = do
  status <- initialStatus program
  advanceProgram program status
  finalBuffer status
