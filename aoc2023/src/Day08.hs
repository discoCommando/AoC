module Day08 where

import Common
import Control.Monad (join)
import Data.Data
import qualified Data.Map as Map
import Data.Set (Set, empty, insert, member)
import Parseable
import Queue
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data Instruction = L | R
  deriving stock (Generic, Show, Eq)

type Network = Map.Map String (String, String)

data Input = Input
  { instructions :: [Instruction],
    network :: Network
  }
  deriving stock (Generic, Show, Eq)

pInstruction :: Parser Instruction
pInstruction =
  Mega.choice
    [ L <$ Mega.char 'L',
      R <$ Mega.char 'R'
    ]

pLine :: Parser (String, (String, String))
pLine = do
  from <- Mega.some Mega.alphaNumChar
  Mega.chunk " = ("
  toL <- Mega.some Mega.alphaNumChar
  Mega.chunk ", "
  toR <- Mega.some Mega.alphaNumChar
  Mega.chunk ")"
  pure (from, (toL, toR))

inputParser :: Parser Input
inputParser = do
  instructions <- Mega.many pInstruction
  Mega.newline >> Mega.newline
  network <- Map.fromList <$> Mega.sepEndBy pLine Mega.newline
  pure $ Input {..}

solution :: Solution Input Int Int
solution =
  Solution
    { parse = inputParser, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

countSteps :: (String -> Bool) -> Input -> String -> [Instruction] -> Int
countSteps fEnd input current instructionsLeft =
  if fEnd current
    then 0
    else case instructionsLeft of
      [] -> undefined -- should not be possible
      (i : is) ->
        let (toL, toR) = input.network Map.! current
         in case i of
              L -> 1 + countSteps fEnd input toL is
              R -> 1 + countSteps fEnd input toR is

part1' :: Input -> Int
part1' input =
  --  countSteps (== "ZZZ") input "AAA" (join $ repeat input.instructions)
  const 1 input

findPath :: Input -> Queue (String, [Instruction], Set String) -> String -> [Instruction]
findPath input queue to =
  let Just ((first, ins, visited), end) = pop queue
   in if first == to
        then ins
        else
          let (toL, toR) = input.network Map.! first
              toLQ = if member toL visited then end else (push (toL, L : ins, insert toL visited) end)
              toRQ = if member toR visited then toLQ else (push (toR, R : ins, insert toR visited) toLQ)
           in findPath input toRQ to

part2' :: Input -> Int
part2' input =
  let as = logMe' "as" $ filter (\s -> head (reverse s) == 'A') $ Map.keys input.network
      fEnd s2 = (head (reverse s2) == 'Z')
      counts = logMe' "counts" $ (\s -> countSteps (fEnd) input s (join $ repeat input.instructions)) <$> as
   in foldl nww (head counts) counts

-- let as = logMe' "as" $ filter (\s -> head (reverse s) == 'A') $ Map.keys input.network
--     paths = logMe' "paths" $ (\s -> findPath input (Queue.fromList [(s, [], empty)]) $ logEmpty' "end" (s,) $ reverse (('Z') : tail (reverse s))) <$> as
--  in length paths

main =
  aoc
    "08"
    solution
