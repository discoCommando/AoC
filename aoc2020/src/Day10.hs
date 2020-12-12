module Day10 where

import Common
import Control.Lens
import Control.Monad.State
import Data.Data
import Data.Functor.Identity
import qualified Data.List
import qualified Data.Map.Strict as DM
import qualified Data.Sequence as DS
import qualified Debug.Trace
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

newtype InputLine = InputLine {jolts :: Integer}
  deriving stock (Generic, Show)
  deriving newtype (Ord, Eq, Num)

inputParser :: Parser InputLine
inputParser = InputLine <$> Mega.decimal

solution :: Solution [InputLine] Int Integer
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

part1' :: [InputLine] -> Int
part1' ls = do
  let sls = Data.List.sort ls
  let highest = last sls
  let s = DS.fromList sls
  let all = s DS.|> (highest + 3)
  let diffs = DS.zipWith (-) all (0 DS.<| all)
  let ones = DS.length $ DS.filter (== 1) diffs
  let threes = DS.length $ DS.filter (== 3) diffs
  ones * threes

part2' :: [InputLine] -> Integer
part2' ls = do
  let sls = Data.List.sort ls
  let highest = last sls
  let s = DS.fromList sls
  let all = s DS.|> (highest + 3)
  let diffs = DS.zipWith (-) all (0 DS.<| all)
  let initState = FibonacciState $ DM.fromList [(0, 1), (1, 1), (2, 2), (3, 4)]
  let groups = DS.foldlWithIndex (\(x : xs) _ v -> if v == 3 then 0 : x : xs else (x + 1) : xs) [0] diffs
  product $ evalState (traverse ultraFib groups) initState

newtype FibonacciState = FibonacciState {m :: DM.Map Integer Integer} deriving stock (Generic)

ultraFib :: Integer -> State FibonacciState Integer
ultraFib a = do
  uses #m (DM.lookup a) >>= \case
    Nothing -> do
      val <- sum <$> traverse ultraFib [a - 1, a - 2, a - 3]
      #m %= DM.insert a val
      pure val
    Just x -> pure x

main =
  aoc
    "10"
    solution
