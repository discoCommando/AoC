module Day01 where

import Common
import Data.Data
import Data.Either (isLeft, isRight)
import Data.List (group, groupBy, sort)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Debug.Trace (traceShowId)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type InputLine' = ToInt (Some Digit) <||> Pure ()

newtype InputLine = InputLine
  { line :: Either Int ()
  }
  deriving stock (Generic, Show, Eq)

inputParser :: Parser InputLine
inputParser = InputLine <$> parser (Proxy :: Proxy InputLine')

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

part1' :: [InputLine] -> Int
part1' input =
  let (max', current) = foldl go (0, 0) input
   in max current max'
  where
    go :: (Int, Int) -> InputLine -> (Int, Int)
    go (max, current) (InputLine (Left i)) = (max, current + i)
    go (max', current) (InputLine (Right ())) = (max current max', 0)

part2' :: [InputLine] -> Int
part2' =
  sum . take 3 . reverse . sort . fmap part1'
    . groupBy
      ( \(InputLine a) (InputLine b) ->
          isLeft a && isLeft b
      )

main =
  aoc
    "01"
    solution
