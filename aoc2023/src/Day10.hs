module Day10 where

import Board
import Common
import Control.Lens ((%=), (.=))
import Control.Monad.ST
import Control.Monad.State
import Data.Data
import Data.Maybe (fromMaybe, mapMaybe)
import Parseable
import Queue
import STState
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data Cell = Empty | Pipe PipeInfo | Start
  deriving stock (Generic, Show, Eq)

data PipeInfo = PipeInfo
  { end1 :: Direction,
    end2 :: Direction,
    distance :: Maybe Int
  }
  deriving stock (Generic, Show, Eq)

pCell :: Parser Cell
pCell = do
  Mega.choice
    [ Empty <$ Mega.char '.',
      Start <$ Mega.char 'S',
      Pipe <$> pPipeInfo
    ]

pPipeInfo :: Parser PipeInfo
pPipeInfo =
  f
    [ -- \| is a vertical pipe connecting north and south.
      -- - is a horizontal pipe connecting east and west.
      -- L is a 90-degree bend connecting north and east.
      -- J is a 90-degree bend connecting north and west.
      -- 7 is a 90-degree bend connecting south and west.
      -- F is a 90-degree bend connecting south and east.
      ("|", North, South),
      ("-", West, East),
      ("L", North, East),
      ("J", North, West),
      ("7", South, West),
      ("F", South, East)
    ]
  where
    f :: [(String, Direction, Direction)] -> Parser PipeInfo
    f xs = Mega.choice $ map (\(s, d1, d2) -> PipeInfo d1 d2 Nothing <$ Mega.string s) xs

data InputLine = InputLine {line :: [Cell]} deriving stock (Generic, Show, Eq)

inputParser :: Parser InputLine
inputParser = InputLine <$> Mega.many pCell

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

data State1 s = State1
  { board :: STBoard s Cell,
    visited :: Visited,
    queue :: Queue (Position, Int)
  }
  deriving stock (Generic)

part1Helper :: STState (State1 s) s Int
part1Helper = do
  q <- gets (.queue)
  case pop q of
    -- we are at the end
    Nothing -> do
      board <- gets (.board)
      list <- toList board
      r <- printBoard board $ \case
        Empty -> "."
        Pipe pipeInfo -> case pipeInfo.distance of
          Nothing -> "0"
          Just i -> show i
        Start -> "S"
      pure
        $ logEmpty' "result" (const r)
        $ maximum
        $ mapMaybe
          ( \case
              Pipe pipeInfo -> pipeInfo.distance
              _ -> Nothing
          )
        $ join list
    Just ((pos, i), q') -> do
      #queue .= q'
      visited <- gets (.visited)
      if isVisited pos visited
        then pure ()
        else do
          #visited .= visit pos visited
          board <- gets (.board)
          cell <- unsafeGet pos board
          let canMoveTo p pipeInfo =
                let dir = p `theDirectionOf` pos
                 in (pipeInfo.end1 == dir || pipeInfo.end2 == dir)
              tryOut pos' otherCell =
                case otherCell of
                  Empty -> pure ()
                  Pipe pipeInfo -> do
                    when (canMoveTo pos' pipeInfo) $ do
                      #queue %= push (pos', i + 1)
                  Start -> pure ()
          case cell of
            Empty -> undefined
            Start -> do
              positions <- getPossiblePositions pos WithoutDiagonals board
              forM_ positions $ uncurry tryOut
            Pipe pipeInfo -> do
              unsafeSet pos (Pipe pipeInfo {distance = Just i}) board
              forM_ [pipeInfo.end1, pipeInfo.end2] $ \dir -> do
                let newPos = performMove dir pos
                newCell <- unsafeGet newPos board
                tryOut newPos newCell
              pure ()
      part1Helper

part1' :: [InputLine] -> Int
part1' is = runST $ do
  board <- newBoard $ map (.line) is
  let state = State1 board mempty mempty
  (v, _) <- initSTState state $ do
    board <- gets (.board)
    mpos <- findBoard board $ (== Start)
    #queue .= push (fromMaybe (Position 0 0) mpos, 0) mempty
    part1Helper
  pure v

part2' :: [InputLine] -> Int
part2' = const 1

main =
  aoc
    "10"
    solution
