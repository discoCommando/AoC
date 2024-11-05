{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Day10 where

import Board
import Common
import Control.Lens ((.=))
import Control.Monad (when)
import qualified Control.Monad.State as State
import Data.Data
import Data.Functor (($>))
import Data.List (nub)
import qualified Data.List as List
import Debug.Trace (trace, traceShowId)
import Parseable
import qualified Parser
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega
import Text.Megaparsec.Debug (dbg)
import Prelude hiding (cycle)

type InputLine = Cmd

data Addx = Addx {delay :: Int, value :: Int}
  deriving stock (Show)

data Cmd = NoOp | Addx' Addx
  deriving stock (Show)

inputParser :: Parser InputLine
inputParser =
  Mega.choice
    [ Mega.chunk "noop" $> NoOp,
      do
        Mega.chunk "addx "
        Addx' . Addx 1 <$> Parser.int
    ]

cycles :: [Int]
cycles = [20, 60 .. 220]

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

cycle :: Cmd -> Int -> (Maybe Cmd, Int)
cycle cmd i = case cmd of
  NoOp -> (Nothing, i)
  Addx' addx -> if addx.delay > 0 then (Just $ Addx' addx {delay = addx.delay - 1}, i) else (Nothing, i + addx.value)

cycleUntilEnd :: [Cmd] -> Int -> [Int]
cycleUntilEnd [] i = []
cycleUntilEnd (first : rest) i =
  let (mcmd, v) = cycle first i
      rest' = maybe rest (: rest) mcmd
   in v : cycleUntilEnd rest' v

part1' :: [InputLine] -> Int
part1' (traceShowId -> input) =
  let cycles' = traceShowId $ cycleUntilEnd input 1
      cycleValues = filter (\(i, _) -> i `elem` cycles) $ fmap (\(i, b) -> (i + 2, b)) $ indexed cycles'
   in sum $ traceShowId $ fmap (uncurry (*)) $ traceShowId cycleValues

positions :: [Position]
positions = do
  y <- [0 .. 5]
  x <- [0 .. 39]
  pure $ Position x y

type SpriteWidths = [Int]

spritePositions :: SpriteWidths -> Position -> [Position]
spritePositions sws (Position _ y) = fmap (\x -> Position (Width x) y) sws

type Hashes = [Position]

data State2 = State2
  { spriteWidths :: SpriteWidths,
    hashes :: Hashes,
    cmds :: [Cmd],
    currentPosition :: [Position]
  }
  deriving stock (Generic)

type State' a = State.State State2 a

getCurrentSprites :: Position -> State' [Position]
getCurrentSprites currentPos = do
  state <- State.get
  pure $ spritePositions state.spriteWidths currentPos

getCurrentPos :: State' Position
getCurrentPos = do
  state <- State.get
  let currentPos = head state.currentPosition
  #currentPosition .= tail state.currentPosition
  pure currentPos

cycle2 :: State' ()
cycle2 = do
  state <- State.get
  case state.cmds of
    [] -> pure ()
    (first : rest) -> do
      currentPos <- getCurrentPos
      currentSprites <- getCurrentSprites currentPos
      when (currentPos `elem` currentSprites) $
        #hashes .= (currentPos : state.hashes)
      case first of
        NoOp ->
          #cmds .= rest
        Addx' addx ->
          if addx.delay == 0
            then do
              #cmds .= rest
              #spriteWidths .= fmap (+ addx.value) state.spriteWidths
            else do
              #cmds .= (Addx' (addx {delay = addx.delay - 1}) : rest)
      cycle2

part2' :: [InputLine] -> Int
part2' input =
  let initialState =
        State2
          { spriteWidths = [0, 1, 2],
            hashes = [],
            cmds = input,
            currentPosition = positions
          }
   in flip State.evalState initialState $ do
        cycle2
        state <- State.get
        let boardPrint = printMap $ traceShowId $ nub state.hashes
        pure $ seq (trace boardPrint 1) 1

main =
  aoc
    "10"
    solution
