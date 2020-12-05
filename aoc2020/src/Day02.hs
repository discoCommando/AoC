module Day02 where

import Common
import Control.Lens
import GHC.Generics
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MegaC
import qualified Text.Megaparsec.Char.Lexer as MegaCL

data Policy = Policy
  { min :: Int,
    max :: Int,
    letter :: Char
  }
  deriving stock (Show, Generic, Eq)

data InputLine = InputLine
  { policy :: Policy,
    password :: String
  }
  deriving stock (Show, Generic, Eq)

policyParser :: Parser Policy
policyParser = do
  min <- MegaCL.decimal
  _ <- MegaC.char '-'
  max <- MegaCL.decimal
  _ <- MegaC.space
  letter <- MegaC.lowerChar
  pure Policy {..}

parser :: Parser InputLine
parser = do
  policy <- policyParser
  MegaC.char ':' >> MegaC.space
  MegaC.space
  password <- takeWhen MegaC.lowerChar
  pure InputLine {..}

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = listParser MegaC.newline parser, -- No parsing required.
      part1 = solution1,
      part2 = solution2
    }

main =
  aoc
    "02"
    solution

validInput :: InputLine -> Bool
validInput i =
  let f = length $ filter (== i ^. #policy . #letter) $ i ^. #password
   in f >= (i ^. #policy . #min) && f <= i ^. #policy . #max

solution1 :: [InputLine] -> Int
solution1 = length . filter (== True) . fmap validInput

validInput2 :: InputLine -> Bool
validInput2 i =
  let x = view #password i `unsafeGet'` (view (#policy . #min) i - 1)
      y = view #password i `unsafeGet'` (view (#policy . #max) i - 1)
      l = i ^. #policy . #letter
   in (x == l || y == l) && (x /= y)

solution2 :: [InputLine] -> Int
solution2 = length . filter (== True) . fmap validInput2
