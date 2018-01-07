
module Day25TuringParser where

import Day25Turing
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token
import Lib hiding (digit)
import Data.Char
import qualified Data.Vector as V

-- could also be expressed as (integer <&> fromIntegral)
readInt :: Parser Int
readInt = do
  sign <- option ' ' (char '-')
  digits <- many1 digit
  pure $ read (sign : digits)

parseProgram :: Parser (Program s)
parseProgram = Program <$> parseInitialStatus <*> parseNumSteps <*> (parseSteps <&> V.fromList)

parseInitialStatus :: Parser StepName
parseInitialStatus = string "Begin in state " *> readStepName <* string ".\n"

parseNumSteps :: Parser Int
parseNumSteps
  =  string "Perform a diagnostic checksum after "
  *> readInt
  <* string " steps.\n\n"

readStepName :: Parser StepName
readStepName = upper <&> ord <&> subtract (ord 'A') <&> StepName

parseSteps :: Parser [Step s]
parseSteps = sepBy1 parseStep (string "\n")

parseStep :: Parser (Step s)
parseStep
  =   Step
  <$> parseStepName
  <*> (parseFunction <?> "failed on the false function")
  <*> (parseFunction <?> "failed on the true function")

parseStepName :: Parser StepName
parseStepName = string "In state " *> readStepName <* string ":\n"

parseFunction :: Parser (Function s)
parseFunction = do
  string "If the current value is "
  digit -- we ignore this digit as we're assuming it from surrounding context
  string ":\n"
  many parseInstruction <?> "instructions for a function"

parseInstruction :: Parser (Instruction s)
parseInstruction = do
  string "- " <?> "leading hyphen for an instruction"
  parseInstructionWrite <|> parseInstructionMove <|> parseInstructionNextStep <?> "expected some kind of instruction"

parseInstructionWrite :: Parser (Instruction s)
parseInstructionWrite = do
  string "Write the value "
  value <- readInt
  string ".\n"
  pure $ instructionWrite value

parseInstructionMove :: Parser (Instruction s)
parseInstructionMove = do
  string "Move one slot to the "
  direction <- many1 (satisfy (/= '.'))
  string ".\n"
  case direction of
    "left"  -> pure $ instructionMove (subtract 1)
    "right" -> pure $ instructionMove (+ 1)
    _       -> error $ "direction " ++ direction ++ " is just no good"

parseInstructionNextStep :: Parser (Instruction s)
parseInstructionNextStep = do
  string "Continue with state "
  name <- readStepName
  string ".\n"
  pure $ instructionNextStep name
