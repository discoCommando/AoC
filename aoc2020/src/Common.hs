{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- taken from https://github.com/blinry/advent-of-code-2019/blob/master/Common.hs

module Common (module X, Solution (..), benchmark, aoc, tbd, toInt, listParser, takeWhen, unsafeMaybe, unsafeParseExample, Parser, unsafeGet) where

import Control.Exception
import qualified Control.Lens as X
import Data.Functor
import Data.Generics.Labels ()
import Data.Generics.Product as X hiding (IsList, list)
import Data.Generics.Sum as X
import Data.Void
import Formatting
import Formatting.Clock
import GHC.Base (Alternative ((<|>)))
import GHC.Exts as X (IsList)
import Paths_aoc2020
import System.Clock
import qualified Text.Megaparsec as Mega

data Solution a b c = Solution
  { parse :: Parser a,
    part1 :: a -> b,
    part2 :: a -> c
  }

benchmark :: IO a -> IO a
benchmark action = do
  start <- getTime Monotonic
  a <- action
  end <- getTime Monotonic
  fprint (" (" % timeSpecs % ")\n") start end
  pure a

aoc :: (Show b, Show c) => String -> Solution a b c -> IO ()
aoc n solution = do
  fileName <- getDataFileName $ "data/" ++ n ++ ".txt"
  input <- readFile fileName
  let problem' = Mega.parse (parse solution) "input" input
  problem <- benchmark $ do
    putStr "Parsing input..."
    a <- evaluate problem'
    case a of
      Left e -> throwIO $ toException e
      Right a' -> pure a'
  benchmark $ putStr $ "Part 1: " ++ show (part1 solution problem)
  benchmark $ putStr $ "Part 2: " ++ show (part2 solution problem)

tbd :: a -> String
tbd _x = "(not implemented)"

-- freqs :: Ord a => [a] -> Map a Integer
-- freqs = Map.fromListWith (+) . map (,1)

toInt :: String -> Int
toInt = read

type Parser a = Mega.Parsec Void String a

listParser :: Parser a -> Parser delimiter -> Parser [a]
listParser p s =
  ( do
      a <- Mega.try p
      (Mega.try s >> pure ()) <|> pure ()
      (a :) <$> listParser p s
  )
    <|> pure []

takeWhen :: Parser a -> Parser [a]
takeWhen p =
  ((:) <$> Mega.try p <*> takeWhen p) <|> pure []

unsafeMaybe :: Maybe a -> a
unsafeMaybe = \case
  Nothing -> undefined
  Just a -> a

unsafeParseExample :: Solution a b c -> String -> a
unsafeParseExample s =
  unsafeMaybe . Mega.parseMaybe (parse s)

unsafeGet :: [a] -> Int -> a
unsafeGet [] _ = undefined
unsafeGet (x : xs) l = if l == 0 then x else unsafeGet xs (l - 1)
