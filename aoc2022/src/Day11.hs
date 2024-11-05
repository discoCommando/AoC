module Day11 where

import Board (applyF)
import Common
import Data.Data
import Data.Function ((&))
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShow, traceShowId)
import Parseable
import qualified Parser
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega
import Text.Megaparsec.Debug (dbg)

type InputLine = Monkey

data Token = Old | Const Integer
  deriving stock (Show)

data OperationType = Mult | Add
  deriving stock (Show)

type Operation = (OperationType, [Token])

newtype Test = DivisibleBy Integer
  deriving stock (Show)

data Throw = Throw {ifTrue :: Int, ifFalse :: Int}
  deriving stock (Show)

data Monkey = Monkey
  { id :: Int,
    items :: [Integer],
    operation :: Operation,
    test :: Test,
    throw :: Throw,
    inspected :: Integer
  }
  deriving stock (Show)

idParser :: Parser Int
idParser = do
  Parser.chunk "Monkey"
  v <- Parser.int
  Parser.chunk ":"
  pure v

startingItemsParser :: Parser [Integer]
startingItemsParser = do
  Parser.chunk "Starting items:"
  Parser.sepBy (Parser.chunk ", ") Parser.int

operationParser :: Parser Operation
operationParser = do
  Parser.chunk "Operation: new ="
  token1 <- tokenParser
  operation <- operationTypeParser
  token2 <- tokenParser
  pure (operation, [token1, token2])
  where
    operationTypeParser :: Parser OperationType
    operationTypeParser =
      Parser.choice
        [ Mult <$ Parser.chunk "*",
          Add <$ Parser.chunk "+"
        ]

    tokenParser :: Parser Token
    tokenParser = do
      Parser.choice
        [ Old <$ Parser.chunk "old",
          Const <$> Parser.int
        ]

testParser :: Parser Test
testParser = do
  Parser.chunk "Test: divisible by"
  DivisibleBy <$> Parser.int

throwParser :: Parser Throw
throwParser = do
  Parser.chunk "If true: throw to monkey "
  ifTrue <- Parser.int
  Parser.chunk "If false: throw to monkey "
  ifFalse <- Parser.int
  pure $ Throw {..}

inputParser :: Parser InputLine
inputParser = dbg "1" $ do
  id <- idParser
  items <- startingItemsParser
  operation <- operationParser
  test <- testParser
  throw <- throwParser
  let inspected = 0
  pure Monkey {..}

solution :: Solution [InputLine] Integer Integer
solution =
  Solution
    { parse = Mega.some inputParser, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

type Monkeys = Map.Map Int Monkey

data Op = DivBy Integer | ModBy Integer

op :: Op -> Integer -> Integer
op op' i = case op' of
  DivBy i2 -> i `div` i2
  ModBy i2 -> i `mod` i2

operation' :: Op -> Operation -> Integer -> Integer
operation' op' (opType, tokens) v =
  let evaluatedTokens = (flip token v <$> tokens)
   in op op' $
        ( case opType of
            Mult -> product
            Add -> sum
        )
          evaluatedTokens
  where
    token :: Token -> Integer -> Integer
    token = \case
      Old -> Prelude.id
      Const v -> const v

test' :: Test -> Throw -> Integer -> Int
test' test throw i = case test of
  DivisibleBy i' ->
    if i `mod` i' == 0 then throw.ifTrue else throw.ifFalse

getDiv :: Test -> Integer
getDiv (DivisibleBy i) = i

divBy3 :: Integer -> Integer
divBy3 i = i `div` 3

runMonkey :: Op -> Int -> Monkeys -> Monkeys
runMonkey op id monkeys =
  let monkey = monkeys Map.! id
      monkeyPath =
        fmap
          (\i -> let res = operation' op monkey.operation i in (res, test' monkey.test monkey.throw res))
          monkey.items
   in monkeys
        & updateAt (\m -> m {items = [], inspected = fromIntegral (length monkey.items) + monkey.inspected}) monkey.id
        & flip (foldr (\(i, id') -> updateAt (\m -> m {items = i : m.items}) id')) monkeyPath
  where
    updateAt :: (Monkey -> Monkey) -> Int -> Monkeys -> Monkeys
    updateAt monkeF =
      Map.update (Just . monkeF)

cycle' :: Op -> Monkeys -> Monkeys
cycle' b monkeys =
  foldl (flip $ runMonkey b) (monkeys) (Map.keys monkeys)

type Inspects = Map.Map Int Int

part1' :: [InputLine] -> Integer
part1' input =
  let initialMonkeys = Map.fromList (fmap (\monke -> (monke.id, monke)) input)
      bestof2 = traceShowId $ fmap (\(_, m) -> m.inspected) $ take 2 $ reverse $ sortOn (\(_, m) -> m.inspected) $ Map.toList $ applyF 20 (cycle' $ DivBy 3) initialMonkeys
   in product bestof2

part2' :: [InputLine] -> Integer
part2' input =
  let initialMonkeys = Map.fromList (fmap (\monke -> (monke.id, monke)) input)
      divider = product $ fmap (\m -> getDiv m.test) input
      bestof2 = traceShowId $ fmap (\(_, m) -> m.inspected) $ take 2 $ reverse $ sortOn (\(_, m) -> m.inspected) $ Map.toList $ applyF 10000 (cycle' $ ModBy divider) initialMonkeys
   in product bestof2

main =
  aoc
    "11"
    solution
