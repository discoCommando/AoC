module Day12 where

import Board
import Common
import Control.Lens
import Control.Monad.ST
import Data.Data
import Data.STRef
import qualified Debug.Trace
import Parseable
import STState
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data Move = Dir Direction | Tur Turn | Forward deriving stock (Generic, Show, Eq)

data InputLine = InputLine {move :: Move, val :: Int} deriving stock (Generic, Show, Eq)

inputParser :: Parser InputLine
inputParser =
  InputLine
    <$> Mega.choice
      [ parseStringAs "F" Forward,
        parseStringAs "N" $ Dir North,
        parseStringAs "S" $ Dir South,
        parseStringAs "W" $ Dir West,
        parseStringAs "E" $ Dir East,
        parseStringAs "L" $ Tur TLeft,
        parseStringAs "R" $ Tur TRight
      ]
    <*> Mega.decimal

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

data State1 = State1
  { direction :: Direction,
    position :: Position
  }
  deriving stock (Generic)

part1' :: [InputLine] -> Int
part1' ls = runST $ do
  let initState =
        State1
          { direction = East,
            position = Position 0 0
          }
  stref <- view _2 <$> initSTState initState (traverse part1Helper ls)
  st <- readSTRef stref
  let p = Debug.Trace.traceShowId $ abs $ st ^. #position
  pure $ p ^. #x . #getWidth' + p ^. #y . #getHeight'

part1Helper :: InputLine -> STState State1 s ()
part1Helper i = do
  direction <- use #direction
  case i ^. #move of
    Forward ->
      #position %= (+ (directionToVector direction * fromIntegral (i ^. #val)))
    Dir d ->
      #position %= (+ (directionToVector d * fromIntegral (i ^. #val)))
    Tur t ->
      #direction %= applyF ((i ^. #val) `div` 90) (performTurn t)
  pure ()

data State2 = State2
  { waypoint :: Position,
    position :: Position
  }
  deriving stock (Generic)

part2' :: [InputLine] -> Int
part2' ls = runST $ do
  let initState =
        State2
          { waypoint = Position 10 (-1),
            position = Position 0 0
          }
  stref <- view _2 <$> initSTState initState (traverse part2Helper ls)
  st <- readSTRef stref
  let p = Debug.Trace.traceShowId $ abs $ st ^. #position
  pure $ p ^. #x . #getWidth' + p ^. #y . #getHeight'

part2Helper :: InputLine -> STState State2 s ()
part2Helper i = do
  waypoint <- use #waypoint
  case i ^. #move of
    Forward ->
      #position %= (+ (waypoint * fromIntegral (i ^. #val)))
    Dir d ->
      #waypoint %= (+ (directionToVector d * fromIntegral (i ^. #val)))
    Tur t ->
      #waypoint %= applyF ((i ^. #val) `div` 90) (rotate t)
  pure ()

main =
  aoc
    "12"
    solution
