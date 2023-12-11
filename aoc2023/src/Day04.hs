module Day04 where

import Common
import Control.Monad (guard)
import Data.Data
import qualified Data.Map as Map
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data InputLine = InputLine
  { cardId :: Int,
    winningNumbers :: [Int],
    ourNumbers :: [Int]
  }
  deriving stock (Generic, Show, Eq)

inputParser :: Parser InputLine
inputParser = do
  Mega.chunk "Card" >> spaces
  cardId <- Mega.decimal
  Mega.chunk ":"
  spaces
  winningNumbers <- Mega.sepEndBy Mega.decimal spaces
  Mega.chunk "|" >> spaces
  ourNumbers <- Mega.sepBy Mega.decimal spaces
  pure InputLine {..}

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

-- based on the number
worthiness :: Int -> Int
worthiness 0 = 0
worthiness n = 2 ^ (n - 1)

howManyWinning :: InputLine -> Int
howManyWinning InputLine {..} = length $ do
  ourNumber <- ourNumbers
  guard $ ourNumber `elem` winningNumbers

part1' :: [InputLine] -> Int
part1' = sum . fmap (worthiness . howManyWinning)

type Results = Map.Map Int [Int]

type Power = Map.Map Int Int

resultsToPower :: Results -> Power
resultsToPower results =
  foldl
    ( \p (i, vals) ->
        foldl
          ( \p' i' ->
              Map.update
                ( \a ->
                    let Just p = Map.lookup i p'
                     in Just $ a + p
                )
                i'
                p'
          )
          p
          vals
    )
    initialPower
    $ Map.toList results
  where
    initialPower :: Power
    initialPower = Map.map (const 1) results

createResults :: [InputLine] -> Results
createResults = Map.fromList . fmap (\(id', i) -> (id', [(id' + 1) .. (howManyWinning i + id')])) . zip [1 ..]

part2' :: [InputLine] -> Int
part2' = sum . Map.elems . logMe' . resultsToPower . logMe' . createResults

main =
  aoc
    "04"
    solution
