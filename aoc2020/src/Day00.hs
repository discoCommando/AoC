module Day00 where

import Common
import Data.Data
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type InputLine = Digit

solution =
  Solution
    { parse = parser (Proxy :: Proxy InputLine), -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

part1' = tbd

part2' = tbd

main =
  aoc
    "00"
    solution
