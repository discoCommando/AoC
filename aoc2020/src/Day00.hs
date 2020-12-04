module Day00 where

import Common
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

main =
  aoc
    "00"
    Solution
      { parse = pure (), -- No parsing required.
        part1 = tbd,
        part2 = tbd
      }
