module Day04 where

import Common
import Control.Lens ((^..), (^?), view)
import Control.Lens.Extras (is)
import Data.Maybe (mapMaybe)
import Data.Proxy
import Debug.Trace
import Parseable
import Passport
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega hiding (space)

-- byr (Birth Year)
-- iyr (Issue Year)
-- eyr (Expiration Year)
-- hgt (Height)
-- hcl (Hair Color)
-- ecl (Eye Color)
-- pid (Passport ID)
-- cid (Country ID)

solution :: Solution [[Field FieldName String]] Int Int
solution =
  Solution
    { parse =
        -- Mega.noneOf [Mega.try $
        --  Mega.newline >> Mega.newline],
        Mega.sepEndBy
          (fieldParser (parser (Proxy :: Proxy (Many NotWhitespace))))
          (Mega.choice [Mega.spaceChar, Mega.newline])
          `Mega.sepEndBy` Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

part1' = length . filter id . fmap hasAllFieldNames

part2' = length . filter (is #_Just) . fmap tryCreateValidPassport

main =
  aoc
    "04"
    solution
