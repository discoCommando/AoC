module Day09 where

import Common
import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad.State (State)
import Control.Monad.State.Lazy (evalState)
import Control.Monad.State.Lazy (MonadState (get))
import Control.Monad.State.Strict (execState)
import Data.Data
import qualified Data.Sequence
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

solution :: Solution [Integer] Integer Integer
solution =
  Solution
    { parse = Mega.sepEndBy Mega.decimal Mega.newline,
      part1 = part1' part1PreambleSize,
      part2 = part2' part1PreambleSize
    }

part1PreambleSize = 25

data State1 = State1
  { buffer :: Data.Sequence.Seq Integer,
    stack :: [Integer]
  }
  deriving stock (Show, Generic)

part1' :: Int -> [Integer] -> Integer
part1' preamble input =
  let init =
        State1
          { buffer = Data.Sequence.fromList $ take preamble input,
            stack = drop preamble input
          }
   in evalState part1Helper init

part1Helper :: State State1 Integer
part1Helper = do
  stack <- use #stack
  let h = head stack
  #stack .= tail stack
  buffer <- use #buffer
  if not $ hasSum h buffer
    then pure h
    else do
      #buffer .= (Data.Sequence.drop 1 buffer Data.Sequence.|> h)
      part1Helper

hasSum :: Integer -> Data.Sequence.Seq Integer -> Bool
hasSum i (Data.Sequence.viewl -> l) = case l of
  Data.Sequence.EmptyL -> False
  v Data.Sequence.:< r ->
    is #_Just (Data.Sequence.findIndexL (== (i - v)) r)
      || hasSum i r

data State2 = State2
  { sum :: Integer,
    set :: Data.Sequence.Seq Integer,
    higher :: Data.Sequence.Seq Integer,
    searchedValue :: Integer
  }
  deriving (Generic)

part2' :: Int -> [Integer] -> Integer
part2' preamble input =
  let (set', rest) = Data.Sequence.splitAt 2 $ Data.Sequence.fromList input
      init =
        State2
          { sum = Prelude.sum set',
            set = set',
            higher = rest,
            searchedValue = part1' preamble input
          }
   in evalState part2Helper init

part2Helper :: State State2 Integer
part2Helper = do
  State2 {..} <- get
  if sum == searchedValue
    then do
      let lowest = minimum set
          highest = maximum set
      pure $ lowest + highest
    else
      if sum < searchedValue || Data.Sequence.length set == 2
        then do
          let (v, rest) = Data.Sequence.splitAt 1 higher
          #higher .= rest
          #set .= set Data.Sequence.>< v
          #sum .= sum + Prelude.sum v
          part2Helper
        else do
          let (v, rest) = Data.Sequence.splitAt 1 set
          #set .= rest
          #sum .= sum - Prelude.sum v
          part2Helper

main =
  aoc
    "09"
    solution
