{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- taken from https://github.com/blinry/advent-of-code-2019/blob/master/Common.hs

module Common where

import Control.Exception
import qualified Data.Map as Map
import Data.Map (Map)
import Formatting
import Formatting.Clock
import Linear.V2
import Paths_aoc2020
import System.Clock
import Text.Printf

data Solution a b c = Solution
  { parse :: String -> a,
    part1 :: a -> b,
    part2 :: a -> c
  }

benchmark :: IO a -> IO ()
benchmark action = do
  start <- getTime Monotonic
  action
  end <- getTime Monotonic
  fprint (" (" % timeSpecs % ")\n") start end

aoc :: (Show b, Show c) => String -> Solution a b c -> IO ()
aoc n solution = do
  fileName <- getDataFileName $ "data/" ++ n ++ ".txt"
  input <- readFile fileName
  let problem = parse solution input
  benchmark $ do
    putStr "Parsing input..."
    evaluate problem
  benchmark $ putStr $ "Part 1: " ++ show (part1 solution problem)
  benchmark $ putStr $ "Part 2: " ++ show (part2 solution problem)

tbd :: a -> String
tbd x = "(not implemented)"

freqs :: Ord a => [a] -> Map a Integer
freqs = Map.fromListWith (+) . map (,1)

toInt :: String -> Int
toInt = read
