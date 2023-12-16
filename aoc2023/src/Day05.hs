module Day05 where

import Common
import Control.Exception (Exception (fromException))
import Data.Data
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega
import qualified Text.Megaparsec.Debug as Mega

data InputLine = InputLine {} deriving stock (Generic, Show, Eq)

data Range = Range {start :: Int, destination :: Int, size :: Int} deriving stock (Generic, Show, Eq)

data Mapping = Mapping
  { from :: String,
    to :: String,
    mappings :: [Range]
  }
  deriving stock (Generic, Show, Eq)

data Input = Input
  { seeds :: [Int],
    maps :: [Mapping]
  }
  deriving stock (Generic, Show, Eq)

pMapping :: Parser Mapping
pMapping = do
  fromTo <- Mega.sepBy (Mega.some Mega.alphaNumChar) (Mega.char '-')
  let from = fromTo !! 0
  let to = fromTo !! 2
  spaces >> Mega.chunk "map:" >> Mega.newline
  mappings <- flip Mega.endBy1 (Mega.try ((Mega.newline >> pure ()) <|> Mega.eof)) $ Mega.try $ do
    numbers <- logMe' <$> Mega.sepBy1 (Mega.decimal) spaces
    pure $ Range (numbers !! 0) (numbers !! 1) $ numbers !! 2
  pure Mapping {..}

pInput :: Parser Input
pInput = do
  Mega.chunk "seeds:" >> spaces
  seeds <- Mega.sepBy Mega.decimal spaces
  Mega.newline >> Mega.newline
  maps <- Mega.sepEndBy pMapping Mega.newline
  pure Input {..}

solution :: Solution Input Int Int
solution =
  Solution
    { parse = pInput, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

-- data Destination = Destination
--   { name :: String,
--     range :: [Range]
--   }
--   deriving stock (Generic, Show, Eq)

-- data State = {
--   maps :: Map.Map String
-- }

findMapping :: String -> [Mapping] -> Maybe Mapping
findMapping _ [] = Nothing
findMapping name (m : ms) =
  if name == m.from
    then Just m
    else findMapping name ms

findDestinationValue :: [Range] -> Int -> Int
findDestinationValue [] i = i
findDestinationValue (r : rs) i =
  logEmpty' "findDestinationValue" (\res -> (r, i, res)) $
    if i >= r.destination && i < r.destination + r.size
      then (i - r.destination) + r.start
      else findDestinationValue rs i

findValue :: String -> [Mapping] -> Int -> Int
findValue source mappings i =
  logEmpty' ("findValue " ++ source ++ " " ++ show i) id $
    case findMapping source mappings of
      Nothing -> i
      Just m -> findValue m.to mappings $ findDestinationValue m.mappings i

part1' :: Input -> Int
part1' (logMe' -> input) = minimum $ (\i -> findValue ((.from) $ head input.maps) input.maps i) <$> input.seeds

findDestinationRanges :: [Range] -> (Int, Int) -> [Range]
findDestinationRanges [] _ = []
findDestinationRanges (r : rs) (start, end) =
  if start >= r.destination && start < r.destination + r.size
    then
      Range
        { start = r.start,
          destination = start,
          size = end - start + 1
        }
        : findDestinationRanges rs (r.destination + r.size, end)
    else r : findDestinationRanges rs (start, end)

part2' :: Input -> Int
part2' = const 1

main =
  aoc
    "05"
    solution
