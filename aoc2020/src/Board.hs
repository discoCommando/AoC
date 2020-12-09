{-# LANGUAGE AllowAmbiguousTypes #-}

module Board where

import Common ()
import Control.Lens ((%~), (&), (^.), _2, view)
import Control.Monad (forM_)
import Control.Monad.ST
import Control.Monad.State (StateT)
import Data.Array.Base (MArray (unsafeWrite), unsafeRead)
import Data.Array.ST
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary)

data Direction = North | South | West | East
  deriving stock (Show, Eq, Generic, Bounded, Enum)

data Turn = TLeft | TRight
  deriving stock (Show, Eq, Generic, Bounded, Enum)

performTurn :: Turn -> Direction -> Direction
performTurn turn direction =
  case turn of
    TLeft ->
      case direction of
        North ->
          West
        West ->
          South
        South ->
          East
        East ->
          West
    TRight ->
      case direction of
        North ->
          East
        West ->
          North
        South ->
          West
        East ->
          South

newtype Width = Width {getWidth' :: Int}
  deriving newtype (Num, Ord, Ix, Enum, Real, Integral, Arbitrary)
  deriving stock (Generic, Show, Eq)

newtype Height = Height {getHeight' :: Int}
  deriving newtype (Num, Ord, Ix, Enum, Real, Integral, Arbitrary)
  deriving stock (Generic, Show, Eq)

data Position = Position {x :: Width, y :: Height}
  deriving stock (Show, Generic, Eq, Ord)

performMove :: Direction -> Position -> Position
performMove = \case
  North ->
    #y %~ flip (-) 1
  South ->
    #y %~ (+ 1)
  West ->
    #x %~ flip (-) 1
  East ->
    #x %~ (+ 1)

newtype Visited = Visited {getVisited :: Map.Map Position ()}
  deriving newtype (Show, Eq)
  deriving stock (Generic)

visit :: Position -> Visited -> Visited
visit pos v = v & #getVisited %~ Map.insert pos ()

isVisited :: Position -> Visited -> Bool
isVisited pos v = Map.member pos $ v ^. #getVisited

-- class (Monad m) => Board m a where
--   getWidth :: Int
--   getHeight :: Int
-- init :: [[a]] -> Board m

data SafeToGo = Safe | Unsafe
  deriving stock (Generic, Eq, Show)

class (Monad m) => Board arr e m where
  getWidth :: arr e -> m Width
  getHeight :: arr e -> m Height
  newBoard :: [[e]] -> m (arr e)
  unsafeSet :: Position -> e -> arr e -> m ()
  unsafeGet :: Position -> arr e -> m e

data STBoard s a = STBoard {getSTBoard :: STArray s Height (STArray s Width a), width :: Width, height :: Height}
  deriving stock (Generic, Eq)

instance Board (STBoard s) e (ST s) where
  getWidth = pure . view #width
  getHeight = pure . view #height
  newBoard listBoard = do
    let height = Height $ length listBoard
    let width = Width . length $ head listBoard
    getSTBoard <- newArray_ (0, height)
    forM_ (indexed listBoard) $ \(x, l) -> do
      newBase <- newArray_ (0, width)
      forM_ (indexed l) . uncurry $ unsafeWrite newBase
      unsafeWrite getSTBoard x newBase
    pure $ STBoard {..}
  unsafeGet position board = do
    w <- readArray (board ^. #getSTBoard) $ position ^. #y
    readArray w $ position ^. #x
  unsafeSet position value board = do
    w <- readArray (board ^. #getSTBoard) $ position ^. #y
    writeArray w (position ^. #x) value

indexed :: [e] -> [(Int, e)]
indexed = view _2 . foldl (\(i, acc) e -> (i + 1, (i, e) : acc)) (0, [])

unsafeUpdate :: (Board arr e m) => Position -> (e -> e) -> arr e -> m ()
unsafeUpdate i f b = do
  v <- unsafeGet i b
  unsafeSet i (f v) b

getPossiblePositions :: (Board arr e m) => Position -> arr e -> m [(Position, e)]
getPossiblePositions pos board = do
  width <- getWidth board
  height <- getHeight board
  let neighbours = flip performMove pos <$> [minBound .. maxBound :: Direction]
  let wihoutOutOfBounds =
        filter
          ( \pos' ->
              and $
                ($ pos')
                  <$> [ (<) 0 . view #x,
                        (>=) width . view #x,
                        (<) 0 . view #y,
                        (>=) height . view #y
                      ]
          )
          neighbours
  traverse (\pos' -> (pos',) <$> unsafeGet pos' board) wihoutOutOfBounds

toList :: (Board arr e m) => arr e -> m [[e]]
toList board = do
  width <- getWidth board
  height <- getHeight board
  for [0 .. width - 1] $ \x ->
    for [0 .. height - 1] $ \y ->
      unsafeGet (Position x y) board