
module Day25TuringParser where

import Day25Turing
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as M
import Lib hiding (parseInt, digit)
import Data.Char
import Control.Monad.ST
import Data.STRef

whitespace :: Parser String
whitespace = many1 (tab <|> space <|> newline)

newlines :: Parser String
newlines = many1 newline

-- could also be expressed as (integer <&> fromIntegral)
parseInt :: Parser Int
parseInt = do
  sign <- option ' ' (char '-')
  digits <- many1 digit
  pure $ read (sign : digits)

parseProgram :: Parser (Program s)
parseProgram = Program <$> parseInitialStatus <*> parseNumSteps <*> (parseSteps <&> V.fromList)

parseInitialStatus :: Parser Int
parseInitialStatus = string "Begin in state " *> parseStepName <* string ".\n"

parseNumSteps :: Parser Int
parseNumSteps
  =  string "Perform a diagnostic checksum after "
  *> parseInt
  <* string " steps.\n\n"

parseStepName :: Parser Int
parseStepName = upper <&> ord <&> subtract (ord 'A')

parseSteps :: Parser [Step s]
parseSteps = sepBy1 parseStep (string "\n\n")

parseStep :: Parser (Step s)
parseStep = do
  string "In state "
  name <- parseStepName
  string ":\n"
  falseFunction <- parseFunction
  trueFunction <- parseFunction
  pure $ Step name falseFunction trueFunction

parseFunction :: Parser (Function s)
parseFunction = do
  string "  If the current value is "
  digit -- we ignore this digit as we're assuming it from surrounding context
  string ":\n"
  many parseInstruction

parseInstruction :: Parser (Instruction s)
parseInstruction = do
  string "    - "
  parseInstructionWrite <|> parseInstructionMove <|> parseInstructionNextStep <?> "expected some kind of instruction"

parseInstructionWrite :: Parser (Instruction s)
parseInstructionWrite = do
  string "Write the value "
  value <- parseInt
  string ".\n"
  pure (\status -> readSTRef (index status) >>= (\index -> M.write (buffer status) index value))

parseInstructionMove :: Parser (Instruction s)
parseInstructionMove = do
  string "Move one slot to the "
  direction <- many1 (satisfy (/= '.'))
  string ".\n"
  case direction of
    "left"  -> pure (\status -> modifySTRef (index status) (subtract 1))
    "right" -> pure (\status -> modifySTRef (index status) (+ 1))
    _       -> error $ "direction " ++ direction ++ " is just no good"

parseInstructionNextStep :: Parser (Instruction s)
parseInstructionNextStep = do
  string "Continue with state "
  name <- parseStepName
  string ".\n"
  pure (\status -> writeSTRef (nextStep status) name)
