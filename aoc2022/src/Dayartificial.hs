module Dayartificial where

import Common
import Data.Data
import Parseable
import Parser
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data CompSign = GT | LT | EQ
  deriving stock (Generic, Show, Eq)

data Expr = Comp CompSign Int | InsurerId Int
  deriving stock (Generic, Show, Eq)

data Node = Node
  { insurerId :: Int
  , percent :: Int
  , expressions :: [Expr]
  }
  deriving stock (Generic, Show, Eq)

pInsurerId :: Parser Int
pInsurerId = do
  chunk "I"
  int

pPercent :: Parser Int
pPercent = do
  i <- int
  chunk "%"
  pure i

pExpr :: Parser Expr
pExpr = choice [

               ]

inputParser :: Parser Node
inputParser = do
  insurerId <- pInsurerId
  chunk ":"
  percent <- pPercent
  expressions <- many pExpr
  pure Node {..}

solution :: Solution [Node] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

part1' :: [Node] -> Int
part1' = const 1

part2' :: [Node] -> Int
part2' = const 1

main =
  aoc
    "artificial"
    solution
