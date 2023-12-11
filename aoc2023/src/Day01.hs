module Day01 where

import Common
import Data.Data
import Data.Maybe (mapMaybe)
import qualified Debug.Trace as Debug
import NumberHelper (numberFromList)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega
import Text.Read (readMaybe)

data InputLine = InputLine {input :: String} deriving stock (Generic, Show, Eq)

inputParser :: Parser InputLine
inputParser = InputLine <$> Mega.takeWhileP Nothing (/= '\n')

solution :: Solution [InputLine] Integer Integer
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

digits :: String -> [Int]
digits = Debug.traceShowId . mapMaybe (readMaybe . pure) . Debug.traceShowId

part1' :: [InputLine] -> Integer
part1' = const 1

-- this is part 1 but it does not work for the input of the part 2
-- sum . (fmap $ \i -> let digits' = digits $ i.input in numberFromList $ reverse [(digits' !! 0), (digits' !! (length digits' - 1))])

part2' :: [InputLine] -> Integer
part2' =
  sum
    . ( fmap $ \i ->
          let digits' = lNumbers $ i.input
           in Debug.traceShowId $ numberFromList $ reverse [(digits' !! 0), (digits' !! (length digits' - 1))]
      )

lNumbers :: String -> [Int]
lNumbers =
  Debug.traceShowId
    . mapMaybe id
    . unsafeMaybe
    . Mega.parseMaybe
      ( Mega.many $ do
          Mega.choice
            [ readMaybe . pure <$> Mega.digitChar,
              Just <$> pNumber,
              -- all letters
              Mega.letterChar *> pure Nothing
            ]
      )
    . Debug.traceShowId

pNumber :: Parser Int
pNumber = do
  x <-
    Mega.lookAhead $
      Mega.choice
        [ Mega.try (Mega.chunk "one") *> pure 1,
          Mega.try (Mega.chunk "two") *> pure 2,
          Mega.try (Mega.chunk "three") *> pure 3,
          Mega.try (Mega.chunk "four") *> pure 4,
          Mega.try (Mega.chunk "five") *> pure 5,
          Mega.try (Mega.chunk "six") *> pure 6,
          Mega.try (Mega.chunk "seven") *> pure 7,
          Mega.try (Mega.chunk "eight") *> pure 8,
          Mega.try (Mega.chunk "nine") *> pure 9
        ]
  Mega.alphaNumChar
  pure x

main =
  aoc
    "01"
    solution
