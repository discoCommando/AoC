module Day05 where

import Common
import Control.Exception (Exception (fromException))
import Data.Data
import Interval
import Interval (SingleInterval)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega
import qualified Text.Megaparsec.Debug as Mega
import Prelude hiding (subtract)

data InputLine = InputLine {} deriving stock (Generic, Show, Eq)

data Range = Range {start :: Integer, destination :: Integer, size :: Integer} deriving stock (Generic, Show, Eq)

data Mapping = Mapping
  { from :: String,
    to :: String,
    mappings :: [Range]
  }
  deriving stock (Generic, Show, Eq)

data Input = Input
  { seeds :: [Integer],
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
    numbers <- logMe' "numbers" <$> Mega.sepBy1 (Mega.decimal) spaces
    pure $ Range (numbers !! 0) (numbers !! 1) $ numbers !! 2
  pure Mapping {..}

pInput :: Parser Input
pInput = do
  Mega.chunk "seeds:" >> spaces
  seeds <- Mega.sepBy Mega.decimal spaces
  Mega.newline >> Mega.newline
  maps <- Mega.sepEndBy pMapping Mega.newline
  pure Input {..}

solution :: Solution Input Integer Integer
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

findDestinationValue :: [Range] -> Integer -> Integer
findDestinationValue [] i = i
findDestinationValue (r : rs) i =
  logEmpty' "findDestinationValue" (\res -> (r, i, res)) $
    if i >= r.destination && i < r.destination + r.size
      then (i - r.destination) + r.start
      else findDestinationValue rs i

findValue :: String -> [Mapping] -> Integer -> Integer
findValue source mappings i =
  logEmpty' ("findValue " ++ source ++ " " ++ show i) id $
    case findMapping source mappings of
      Nothing -> i
      Just m -> findValue m.to mappings $ findDestinationValue m.mappings i

part1' :: Input -> Integer
part1' (logMe' "part1Input" -> input) = minimum $ (\i -> findValue ((.from) $ head input.maps) input.maps i) <$> input.seeds

mkSingleInterval' :: Integer -> Integer -> SingleInterval
mkSingleInterval' x y = mkSingleInterval x (x + y - 1)

data Part2 = Part2
  { initialInterval :: Interval,
    mappings :: [Mapping]
  }

i2p2 :: Input -> Part2
i2p2 Input {..} =
  Part2
    { initialInterval = fromList' $ inits seeds,
      mappings = maps
    }
  where
    inits :: [Integer] -> [SingleInterval]
    inits [] = []
    inits (_ : []) = undefined -- should not happen
    inits (x : y : xs) = mkSingleInterval' x y : inits xs

-- intervalMapping :: Mapping -> Interval -> Interval
-- intervalMapping

singleIntervalMapping :: [Range] -> Interval -> Interval
singleIntervalMapping [] i = i
singleIntervalMapping (r : rest) i =
  let source = mkSingleInterval' r.start r.size
      destination = mkSingleInterval' r.destination r.size
      intersection = intersect i $ singleton' destination
      mappedIntersection = mapIntervals (mapSingleInterval destination source) intersection
      subtraction = subtract i $ singleton' $ destination
   in union mappedIntersection $ singleIntervalMapping rest $ subtraction
  where
    mapSingleInterval :: SingleInterval -> SingleInterval -> SingleInterval -> SingleInterval
    mapSingleInterval source destination s =
      mkSingleInterval (destination.start + (s.start - source.start)) (destination.end + (s.end - source.end))

findValues :: String -> [Mapping] -> Interval -> Interval
findValues source mappings i =
  case findMapping source mappings of
    Nothing -> i
    Just m -> findValues m.to mappings $ logEmpty' ("findValues " ++ source ++ " " ++ show i ++ " " ++ show m) id $ singleIntervalMapping m.mappings i
  where

part2' :: Input -> Integer
part2' i =
  let p2 = i2p2 i
   in fst $
        head $
          logMe' "results" $
            toList $
              findValues ((.from) $ head $ maps i) (maps i) $
                p2.initialInterval

main =
  aoc
    "05"
    solution
