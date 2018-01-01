
import qualified Data.Map as Map
import Data.Map(Map)
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Bifunctor
import Data.Maybe(fromMaybe)
import Data.List
import Debug.Trace

tracey _ a = a

(&) :: a -> (a -> b) -> b
(&) = flip ($)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

type Register = String
type Status = Map Register Int

data Instruction = Instruction Operation Condition
data Operation = Operation Register Int
data Condition = Condition Register (Int -> Bool)

main :: IO ()
main = do
  instructions <- readFile "input8.txt" <&> lines <&> map (actuallyParse parseInstruction)
  let results = scanl' (flip executeInstruction) Map.empty instructions
  putStrLn $ show (length instructions)
  putStrLn $ show (length results)
  putStrLn $ show (maxiest results)

maxiest :: [Status] -> Int
maxiest statuses = map maxy statuses & safeMaximum
  where maxy status = Map.toList status <&> snd & safeMaximum
        safeMaximum [] = 0
        safeMaximum list = maximum list

-- choose one fully-parsed result, or crash if there's none
actuallyParse :: ReadP a -> String -> a
actuallyParse reader input
  = readP_to_S reader input & filter (null . snd) & head & fst

executeInstruction :: Instruction -> Status -> Status
executeInstruction (Instruction operation condition) status
  = if checkCondition condition status
        then tracey "yep" $ executeOperation operation status
        else tracey "nup" $ status

checkCondition :: Condition -> Status -> Bool
checkCondition (Condition register func) status
  = fromMaybe 0 (Map.lookup register status) & func

executeOperation :: Operation -> Status -> Status
executeOperation (Operation register amount)
  = Map.insertWith (+) register amount

parseInstruction :: ReadP Instruction
parseInstruction = do
  operation <- parseOperation
  condition <- parseCondition
  pure (Instruction operation condition)

parseOperation :: ReadP Operation
parseOperation = do
  registerName <- word
  char ' '
  operationName <- word
  char ' '
  amount <- word <&> read
  char ' '
  let finalAmount = case operationName of
               "inc" -> amount
               "dec" -> (negate amount)
               _ -> error $ "but idk what operation " ++ operationName ++ " is"
  pure (Operation registerName finalAmount)

parseCondition :: ReadP Condition
parseCondition = do
  string "if "
  registerName <- word
  char ' '
  comparator <- word
  char ' '
  amount <- word <&> read
  eof
  let func = case comparator of
                "<" -> (< amount)
                ">" -> (> amount)
                ">=" -> (>= amount)
                "<=" -> (<= amount)
                "==" -> (== amount)
                "!=" -> (/= amount)
                _ -> error $ "but idk what comparator " ++ comparator ++ " is"
  pure (Condition registerName func)

word :: ReadP String
word = many1 (satisfy (not . isSpace))
