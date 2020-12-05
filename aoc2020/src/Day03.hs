module Day03 where

import Board
import Common
import Control.Lens ((%~), (&), (.=), (.~), (^.), _2)
import Control.Monad.Reader
import Control.Monad.ST (ST, runST)
import Control.Monad.State
import Data.STRef
import Data.Traversable (for)
import Debug.Trace (trace, traceId, traceShow, traceShowId)
import STState
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data Tile = Empty | Tree
  deriving stock (Generic, Show, Eq)

tileParser :: Parser Tile
tileParser =
  Mega.choice [parseStringAs "." Empty, parseStringAs "#" Tree]

solution :: Solution [[Tile]] Int Int
solution =
  Solution
    { parse = Mega.many tileParser `Mega.sepBy` Mega.newline, -- No parsing required.
      part1 = \tiles -> runST $ part1Helper' tiles,
      part2 = \tiles -> runST $ part2Helper slopes2 tiles
    }

main =
  aoc
    "03"
    solution

type Slope = [Direction]

slope1 :: Slope
slope1 = [South, East, East, East]

slopes2 :: [Slope]
slopes2 =
  [ [South, East],
    slope1,
    replicate 5 East ++ [South],
    replicate 7 East ++ [South],
    [East, South, South]
  ]

performMove' :: Width -> Height -> [Direction] -> Position -> Position
performMove' width height dirs pos =
  foldl (flip performMove) pos dirs
    & #x %~ (`mod` width)
    & #y %~ (`mod` height)

data State' s = State'
  { pos :: Position,
    board :: STBoard s Tile,
    slope :: Slope,
    counter :: Int
  }
  deriving stock (Generic)

type AppD03 s a = STState (State' s) s a

part1Helper' :: [[Tile]] -> ST s Int
part1Helper' tiles = do
  board <- newBoard tiles
  stref <-
    newSTRef $
      State'
        { counter = 0,
          pos = Position 0 0,
          slope = slope1,
          board = board
        }
  runSTState part1Helper stref

part1Helper :: AppD03 s Int
part1Helper = do
  State' {..} <- get
  width <- liftS $ getWidth board
  height <- liftS $ getHeight board
  let newPos = performMove' width height slope pos
  if newPos ^. #y < pos ^. #y
    then pure counter
    else do
      t <- liftS $unsafeGet newPos board
      let v = if t == Tree then 1 else 0
      #pos .= newPos
      #counter .= counter + v
      part1Helper

part2Helper :: [Slope] -> [[Tile]] -> ST s Int
part2Helper slopes tiles = do
  board <- newBoard tiles
  stref <-
    newSTRef $
      State'
        { counter = 0,
          pos = Position 0 0,
          slope = slope1,
          board = board
        }
  flip runSTState stref $ do
    fmap (product . traceShowId) . for slopes $ \slope -> do
      #slope .= slope
      #pos .= Position 0 0
      #counter .= 0
      part1Helper
