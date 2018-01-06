
module Day25Turing where

import Control.Monad.ST
import Control.Applicative
import Data.STRef
import Text.ParserCombinators.ReadP
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
                 }

data Step s = Step
              {
                name :: Int,
                falseFunction :: Function s,
                trueFunction :: Function s
              }

type Function s = [Instruction s]
type Instruction s = Status s -> ST s ()

finalBuffer :: Status s -> ST s (V.Vector Int)
finalBuffer status = undefined

initialStatus :: Program s -> ST s (Status s)
initialStatus program
  = liftA3 Status (M.new 10000000) (newSTRef 5000000) (newSTRef $ firstStep program)

parseProgram :: ReadP (Program s)
parseProgram = do
  string "Begin in state "
  initialStatus <- parseStepName
  string ".\n"
  string "Perform a diagnostic checksum after "
  numSteps <- parseInt
  string " steps.\n"
  steps <- parseSteps <&> V.fromList
  pure $ Program initialStatus numSteps steps

parseStepName :: ReadP Int
parseStepName = get <&> ord <&> subtract (ord 'A')

parseSteps :: ReadP [Step s]
parseSteps = sepBy parseStep (string "\n\n")

parseStep :: ReadP (Step s)
parseStep = do
  string "In state "
  name <- parseStepName
  string ":\n"
  falseFunction <- parseFunction
  trueFunction <- parseFunction
  pure $ Step name falseFunction trueFunction

parseFunction :: ReadP (Function s)
parseFunction = do
  string "  If the current value is "
  get -- we ignore this digit as we're assuming it from surrounding context
  string ":\n"
  sepBy parseInstruction (string "\n")

parseInstruction :: ReadP (Instruction s)
parseInstruction = do
  string "    - "
  parseInstructionWrite <++ parseInstructionMove <++ parseInstructionNextStep

parseInstructionWrite :: ReadP (Instruction s)
parseInstructionWrite = do
  string "Write the value "
  value <- parseInt
  string "."
  pure (\status -> readSTRef (index status) >>= (\index -> M.write (buffer status) index value))

parseInstructionMove :: ReadP (Instruction s)
parseInstructionMove = do
  string "Move one slot to the "
  direction <- many1 (satisfy (/= '.'))
  get
  case direction of
    "left"  -> pure (\status -> modifySTRef (index status) (subtract 1))
    "right" -> pure (\status -> modifySTRef (index status) (+ 1))
    _       -> error $ "direction " ++ direction ++ " is just no good"

parseInstructionNextStep :: ReadP (Instruction s)
parseInstructionNextStep = do
  string "Continue with state "
  name <- parseStepName
  string "."
  pure (\status -> writeSTRef (nextStep status) name)

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