{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Day2108 where

import Common
import Data.Data
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace (traceShowId)
import Numeric (numberFromList)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type Code' = Some Letter

type Codes' = SepEndBy Code' Space

type InputLine' = (Codes' <$ Chunk "| ") :+: Codes'

type Letters = Set.Set Char

data InputLine = InputLine {helpers :: [Letters], code :: [Letters]} deriving stock (Generic, Show, Eq)

inputParser :: Parser InputLine
inputParser = do
  (helpers, code) <- parser @InputLine' Proxy
  pure InputLine {helpers = Set.fromList <$> helpers, code = Set.fromList <$> code}

solution :: Solution [InputLine] Int Integer
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

letters :: Map.Map Int Letters
letters =
  Map.fromList $
    (\(i, l) -> (i, Set.fromList l))
      <$> [ (0, "abcefg"),
            (1, "cf"),
            (2, "acdeg"),
            (3, "acdfg"),
            (4, "bcdf"),
            (5, "abdfg"),
            (6, "abdefg"),
            (7, "acf"),
            (8, "abcdefg"),
            (9, "abcdfg")
          ]

type PossibleTranslation = Map.Map Char Letters

type Translation = Map.Map Char Char

allLetters = Set.fromList ['a' .. 'g']

initialMap :: PossibleTranslation
initialMap =
  Map.fromList
    [ (c, allLetters) | c <- Set.toList allLetters
    ]

getCandidates :: Letters -> [Letters]
getCandidates c =
  fmap snd $
    filter (\(i, s) -> length c == length s) $
      Map.toList letters

getPossibleChars :: Letters -> Letters
getPossibleChars =
  Set.unions . getCandidates

getMissingLetters :: Letters -> Letters
getMissingLetters = Set.difference allLetters

getPossibleCharsFromMissingLetters :: Letters -> (Letters, Letters)
getPossibleCharsFromMissingLetters cs =
  let candidates = getMissingLetters <$> getCandidates cs
   in (getMissingLetters cs, Set.unions candidates)

deleteFromAllIfSingle :: Char -> PossibleTranslation -> PossibleTranslation
deleteFromAllIfSingle c pt =
  let rc = pt Map.! c
   in if length rc > 1
        then pt
        else
          let rcc = head $ Set.toList rc
           in Map.mapWithKey (\k -> if k == c then id else Set.delete rcc) pt

figureOutLetters :: [Letters] -> PossibleTranslation -> Translation
figureOutLetters [] m =
  let polishedSet = foldl (flip deleteFromAllIfSingle) m $ Set.toList allLetters
   in Map.map (head . Set.toList) polishedSet
figureOutLetters (f : rest) m =
  let possibleLetters = getPossibleChars f
      newMap = foldl (\m' c -> Map.update (Just . Set.intersection possibleLetters) c m') m f
      (missing, missingPossibleLetters) = getPossibleCharsFromMissingLetters f
      newMap' = foldl (\m' c -> Map.update (Just . Set.intersection missingPossibleLetters) c m') newMap missing
   in figureOutLetters rest newMap'

figureOutNumber :: Translation -> Letters -> Int
figureOutNumber translation cs =
  let translated = Set.map (translation Map.!) cs
   in fst . head $ filter ((== translated) . snd) $ Map.toList letters

figureOutCode :: InputLine -> [Int]
figureOutCode InputLine {..} =
  let figuredOutLetters = figureOutLetters helpers initialMap
      translatedCode = figureOutNumber figuredOutLetters <$> code
   in translatedCode

part1Numbers = [1, 4, 7, 8]

part1' :: [InputLine] -> Int
part1' = sum . fmap ((length . filter (`elem` part1Numbers)) . figureOutCode)

part2' :: [InputLine] -> Integer
part2' = sum . fmap (numberFromList . reverse . figureOutCode)

main =
  aoc
    "2108"
    solution
