module Day08 where

import Board
import Common
import Control.Lens (Identity (runIdentity))
import Control.Lens.Combinators (use)
import Control.Lens.Setter ((.=))
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Data
import Data.Foldable (for_)
import Data.List
import qualified Data.Map as Map
import Data.Traversable (for)
import Debug.Trace (traceShowId)
import Parseable
import qualified Queue
import STState
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data Cell = Cell {value :: Int, visible :: CellState}
  deriving stock (Generic, Show, Eq)

type InputLine = [Cell]

inputParser :: Parser InputLine
inputParser = Mega.some $ do
  value <- read . singleton <$> Mega.digitChar
  pure Cell {visible = NotVisible, ..}

data CellState = Visible | NotVisible
  deriving stock (Generic, Show, Eq)

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

newtype State1 s = State1
  { board :: STBoard s Cell
  }
  deriving stock (Generic)

{- we know that all the previous cells are visible
   -}
viewFromTheSide :: Position -> Direction -> Int -> STState (State1 s) s ()
viewFromTheSide current direction max' = do
  board <- use #board
  currentValue <- getAt current board
  case currentValue of
    Nothing -> pure ()
    Just currentValue -> do
      let reverseVector = directionToVector $ reverseDirection direction
      when (max' < currentValue.value) $ do
        unsafeSet current currentValue {visible = Visible} board
      viewFromTheSide (current + directionToVector direction) direction (max max' currentValue.value)

type Visibles = [Position]

simpleViewFromSide :: Int -> [(Position, Cell)] -> Visibles -> Visibles
simpleViewFromSide max' [] vs = vs
simpleViewFromSide max' ((pos, first) : rest) vs =
  let (newVs, newMax) =
        if first.value > max'
          then (pos : vs, first.value)
          else (vs, max')
   in simpleViewFromSide newMax rest newVs

viewAllFromSide :: [[(Position, Cell)]] -> Visibles -> Visibles
viewAllFromSide cells vs =
  foldl' (flip (simpleViewFromSide (-1))) vs cells

viewAll' :: ([[(Position, Cell)]] -> a -> a) -> [[Cell]] -> a -> a
viewAll' f (indexBoard -> cells) vs =
  let all =
        [ cells,
          transpose cells,
          reverse <$> cells,
          reverse <$> transpose cells
        ]
   in foldl' (flip f) vs all

-- viewAll :: [[Cell]] -> Int
-- viewAll (indexBoard -> cells) =
--           length $ nub $ foldl' (flip viewAllFromSide) [] all

-- walkAround :: STState (State1 s) s ()
-- walkAround = do
--   queue <- use #queue
--   visited <- use #visited
--   board <- use #board
--   let next = Queue.pop queue
--   case next of
--     Nothing -> pure ()
--     Just (next, rest) -> do
--       #queue .= rest
--       unless (isVisited next visited) $ do
--         #visited .= visit next visited
--         current <- unsafeGet next board
--         when (current.visible == NotVisible) $ do
--           possiblePositions <- getPossiblePositions next WithoutDiagonals board
--           when (any (\(_, e) -> e.visible == Visible) possiblePositions ) $ do
--             unsafeSet next current {visible = Visible} board
--       walkAround

part1Helper :: STState (State1 s) s Int
part1Helper = do
  board <- use #board
  height <- getHeight board
  width <- getWidth board
  -- let edges = [ [ Position x 0 | x <- [0..width - 1]],
  --               [ Position x (height - 1) | x <- [0..width - 1]],

  --               [ Position (width - 1) y | y <- [0..height - 1]],
  --               [ Position 0 y | y <- [1..height - 1]]
  --             ]
  -- for_ edges $ \l -> for l $ \p -> unsafeUpdate p (\c -> c {visible = Visible}) board
  let views =
        [ (South, [Position x 0 | x <- [0 .. width - 1]]),
          (North, [Position x (height - 1) | x <- [0 .. width - 1]]),
          (West, [Position (width - 1) y | y <- [0 .. height - 1]]),
          (East, [Position 0 y | y <- [0 .. height - 1]])
        ]
  let queue = Queue.fromList
  for_ views $ \(direction, positions) -> do
    for positions $ \startingPosition -> do
      viewFromTheSide startingPosition direction (-1)
  foldM board 0 (\cell _ v -> pure $ v + if cell.visible == Visible then 1 else 0)

part1' :: [InputLine] -> Int
part1' input =
  length $
    nub $
      viewAll'
        viewAllFromSide
        input
        []

-- runST $ do
--   initState <- State1 <$> newBoard input
--   (result, _) <- initSTState initState part1Helper
--   pure result
-- viewAll

type ViewingDistances = Map.Map Position [Int]

viewingDistance :: [(Position, Cell)] -> ViewingDistances -> ViewingDistances
viewingDistance [] vd = vd
viewingDistance ((position, cell) : rest) vd =
  let score' = length $ takeWhile (\(_, cell') -> cell'.value < cell.value) rest
      score = if score' < length rest then score' + 1 else score'
   in viewingDistance rest $ Map.alter (Just . maybe [score] (score :)) position vd

part2' :: [InputLine] -> Int
part2' input =
  maximum $
    fmap (\(_, v) -> product v) $
      Map.toList $
        viewAll' (flip $ foldr viewingDistance) input mempty

main =
  aoc
    "08"
    solution
