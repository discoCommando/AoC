module Day02 where

import Common
import Data.Data
import qualified Debug.Trace as Debug
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data C = R | G | B deriving stock (Show, Eq)

pC :: Parser C
pC = Mega.choice [R <$ Mega.chunk "red", G <$ Mega.chunk "green", B <$ Mega.chunk "blue"]

data Set = Set {r :: Int, g :: Int, b :: Int} deriving stock (Show, Eq)

pSet :: Parser Set
pSet =
  f (Set 0 0 0)
    <$> ( flip Mega.sepBy (Mega.chunk ", ") $ do
            i :: Int <- Mega.decimal
            Mega.space1
            c <- pC
            pure (c, i)
        )
  where
    f :: Set -> [(C, Int)] -> Set
    f s [] = s
    f s ((R, i) : rest) = f (s {r = i}) rest
    f s ((G, i) : rest) = f (s {g = i}) rest
    f s ((B, i) : rest) = f (s {b = i}) rest

pInputLine :: Parser InputLine
pInputLine = do
  id <- Mega.chunk "Game " *> Mega.decimal <* Mega.chunk ": "
  sets <- Mega.sepBy pSet (Mega.chunk "; ")
  pure InputLine {..}

data InputLine = InputLine {sets :: [Set], id :: Int} deriving stock (Generic, Show, Eq)

inputParser :: Parser InputLine
inputParser = pInputLine

conditionSet :: Set
conditionSet = Set 12 13 14

condition :: Set -> Bool
condition Set {..} = r <= conditionSet.r && g <= conditionSet.g && b <= conditionSet.b

conditionInputLine :: InputLine -> Int
conditionInputLine InputLine {..} =
  if all condition sets
    then Debug.trace ("won: " <> show id) id
    else 0

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

part1' :: [InputLine] -> Int
part1' = sum . fmap conditionInputLine . Debug.traceShowId

instance Semigroup Set where
  s1 <> s2 = Set {r = r s1 `max` r s2, g = g s1 `max` g s2, b = b s1 `max` b s2}

instance Monoid Set where
  mempty = Set 0 0 0

power :: Set -> Int
power Set {..} = r * g * b

part2' :: [InputLine] -> Int
part2' inputs = sum $ (\i -> power $ mconcat i.sets) <$> inputs

main =
  aoc
    "02"
    solution
