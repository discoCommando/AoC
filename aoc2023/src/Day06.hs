module Day06 where

import BinSearch
import Board (Position (x))
import Common
import Data.Data
import Data.List (elemIndices)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data Race = Race {time :: Int, distance :: Int}
  deriving stock (Generic, Show, Eq)

inputParser :: Parser [Race]
inputParser = do
  Mega.chunk "Time:" >> spaces
  times <- Mega.sepBy Mega.decimal spaces
  Mega.newline
  Mega.chunk "Distance:" >> spaces
  distances <- Mega.sepBy Mega.decimal spaces
  pure $ (uncurry Race) <$> zip times distances

solution :: Solution [Race] Int Int
solution =
  Solution
    { parse = inputParser, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

-- first approach
raceLength :: Race -> Int -> Int
raceLength race holdTime =
  if race.time <= holdTime
    then 0
    else (race.time - holdTime) * holdTime

allRaceLengths :: Race -> [Int]
allRaceLengths race = logEmpty' "race lengths" id $ raceLength race <$> [0 .. race.time]

findHighestHoldTimes :: Race -> Int
findHighestHoldTimes race =
  let raceLengths = min (race.distance + 1) <$> allRaceLengths race
      maxRaceLength = logEmpty' "maxium" id $ maximum raceLengths
   in logEmpty'
        "race"
        (\res -> (race, res))
        $ length
        $ elemIndices maxRaceLength raceLengths

-- second approach
raceLength' :: Race -> Int -> Int
raceLength' race holdTime =
  min (race.distance + 1) $ raceLength race holdTime

findHighestHoldTimes' :: Race -> Int
findHighestHoldTimes' race =
  let highestValue = race.distance + 1
      -- bin search for the highest possible value - we assume that it's reachable
      -- also, we are lucky that we found it in a way, ie that the middle
      -- index is "on the right side", because the "list" is looks like "/\", ie
      -- only the first part is increasing.
      Just middleIndex = binSearchEq' 0 race.time (raceLength' race) highestValue
      -- we get the index of the biggest value until the middle index that is smaller than
      -- the highest value
      Just (leftIndex, _) =
        logEmpty' "left" id $
          binSearchBiggestUntil' 0 middleIndex (raceLength' race) highestValue
      -- same, but for the right half. But there, we need to negate the raceLength function
      Just (rightIndex, _) =
        logEmpty' "right" id $
          binSearchSmallestUntil' middleIndex race.time ((* (-1)) . raceLength' race) (-highestValue)
   in logEmpty' "result" (,race) $ rightIndex - leftIndex - 1

part1' :: [Race] -> Int
part1' rs = product $ findHighestHoldTimes' <$> rs

part2' :: [Race] -> Int
part2' races =
  let distances = (.distance) <$> races
      times = (.time) <$> races
      toInt' :: [Int] -> Int
      toInt' xs = read $ logMe' "toInt" $ concat $ show <$> xs
   in findHighestHoldTimes' $ Race {distance = toInt' distances, time = toInt' times}

main =
  aoc
    "06"
    solution
