module Board where

import Common (Parser)
import Control.Lens (view, (%~), (&), (^.), _2)
import Control.Monad (foldM, forM_, join)
import Control.Monad.ST
import Control.Monad.State (StateT)
import Data.Array.Base (MArray (unsafeWrite), unsafeRead)
import Data.Array.ST
import Data.Coerce
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Debug.Trace (traceShowId)
import GHC.Generics (Generic)
import Prettyprinter
import Prettyprinter.Internal (renderShowS)
import Prettyprinter.Render.String (renderString)
import Test.QuickCheck (Arbitrary)
import qualified Text.Megaparsec as Mega

directionParser :: [Char] -> Parser Direction
directionParser =
  Mega.choice . fmap (\(d, c) -> Mega.chunk [c] $> d) . zip [minBound .. maxBound :: Direction]

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
          North
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

reverseDirection :: Direction -> Direction
reverseDirection = applyF 2 (performTurn TLeft)

rotate :: Turn -> Position -> Position
rotate t Position {..} =
  case t of
    TLeft ->
      Position (fromIntegral y) $ (-1) * fromIntegral x
    TRight ->
      Position ((-1) * fromIntegral y) $ fromIntegral x

divPosition :: Position -> Position -> Position
divPosition p1 p2 = Position (p1.x `div` p2.x) (p1.y `div` p2.y)

directionToVector :: Direction -> Position
directionToVector = \case
  North -> Position 0 (-1)
  East -> Position 1 0
  West -> Position (-1) 0
  South -> Position 0 1

applyF :: Int -> (a -> a) -> a -> a
applyF 0 _ = id
applyF x f = f . applyF (x - 1) f

newtype Width = Width {getWidth' :: Int}
  deriving newtype (Num, Ord, Ix, Enum, Real, Integral, Arbitrary, Pretty)
  deriving stock (Generic, Show, Eq)

newtype Height = Height {getHeight' :: Int}
  deriving newtype (Num, Ord, Ix, Enum, Real, Integral, Arbitrary, Pretty)
  deriving stock (Generic, Show, Eq)

data Position = Position {x :: Width, y :: Height}
  deriving stock (Show, Generic, Eq, Ord)

instance Pretty Position where
  pretty p = "(" <> pretty p.x <> "," <> pretty p.y <> ")"

instance Num Position where
  p1 + p2 = Position (p1 ^. #x + p2 ^. #x) (p1 ^. #y + p2 ^. #y)
  p1 * p2 = Position (p1 ^. #x * p2 ^. #x) (p1 ^. #y * p2 ^. #y)
  p1 - p2 = Position (p1 ^. #x - p2 ^. #x) (p1 ^. #y - p2 ^. #y)
  abs p = Position (abs $ p ^. #x) (abs $ p ^. #y)
  signum p = Position (signum $ p ^. #x) (signum $ p ^. #y)
  fromInteger v = Position (fromInteger v) (fromInteger v)

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
  deriving newtype (Show, Eq, Semigroup, Monoid)
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
    getSTBoard <- newArray_ (0, height - 1)
    forM_ (indexed listBoard) $ \(x, l) -> do
      newBase <- newArray_ (0, width - 1)
      forM_ (indexed l) . uncurry $ unsafeWrite newBase
      unsafeWrite getSTBoard x newBase
    pure $ STBoard {..}
  unsafeGet position board = do
    w <- readArray (board ^. #getSTBoard) $ position ^. #y
    readArray w $ position ^. #x
  unsafeSet position value board = do
    w <- readArray (board ^. #getSTBoard) $ position ^. #y
    writeArray w (position ^. #x) value

isInBounds :: Board arr e m => Position -> arr e -> m Bool
isInBounds position board = do
  width <- getWidth board
  height <- getHeight board
  return $
    and
      [ position.x >= 0,
        position.x < width,
        position.y >= 0,
        position.y < height
      ]

getAt :: Board arr e m => Position -> arr e -> m (Maybe e)
getAt p a = do
  inBounds <- isInBounds p a
  if inBounds
    then Just <$> unsafeGet p a
    else return Nothing

indexed :: [e] -> [(Int, e)]
indexed = view _2 . Prelude.foldl (\(i, acc) e -> (i + 1, (i, e) : acc)) (0, [])

unsafeUpdate :: (Board arr e m) => Position -> (e -> e) -> arr e -> m ()
unsafeUpdate i f b = do
  v <- unsafeGet i b
  unsafeSet i (f v) b

data PossiblePositionsMode = WithDiagonals | WithoutDiagonals

getPossiblePositions :: (Board arr e m) => Position -> PossiblePositionsMode -> arr e -> m [(Position, e)]
getPossiblePositions pos mode board = do
  width <- getWidth board
  height <- getHeight board
  let neighbours = case mode of
        WithoutDiagonals -> flip performMove pos <$> [minBound .. maxBound :: Direction]
        WithDiagonals -> (+ pos) <$> [Position x y | x <- [-1 .. 1], y <- [-1 .. 1], x /= 0 || y /= 0]
  let wihoutOutOfBounds =
        filter
          ( \pos' ->
              and $
                ($ pos')
                  <$> [ (<=) 0 . view #x,
                        (>) width . view #x,
                        (<=) 0 . view #y,
                        (>) height . view #y
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

indexedMMap :: (Board arr e m) => arr e -> (e -> Position -> m e) -> m ()
indexedMMap board f = do
  width <- getWidth board
  height <- getHeight board
  for_ [0 .. width - 1] $ \x ->
    for [0 .. height - 1] $ \y -> do
      let pos = Position x y
      e <- unsafeGet pos board
      newE <- f e pos
      unsafeSet pos newE board

getAllPositions :: (Board arr e m) => arr e -> m [Position]
getAllPositions board = do
  width <- getWidth board
  height <- getHeight board
  p <- for [0 .. width - 1] $ \x ->
    for [0 .. height - 1] $ \y ->
      pure $ Position x y
  pure $ join p

foldM :: (Board arr e m) => arr e -> acc -> (e -> Position -> acc -> m acc) -> m acc
foldM board acc f = do
  width <- getWidth board
  height <- getHeight board
  Control.Monad.foldM
    ( \acc x ->
        Control.Monad.foldM
          ( \acc' y -> do
              let p = Position x y
              e <- unsafeGet p board
              f e p acc'
          )
          acc
          [0 .. height - 1]
    )
    acc
    [0 .. width - 1]

printBoard :: (Board arr e m, Pretty e) => arr e -> m String
printBoard board = do
  width <- getWidth board
  height <- getHeight board
  p <- for [0 .. height - 1] $ \y -> do
    ss <- for [0 .. width - 1] $ \x -> do
      e <- unsafeGet (Position x y) board
      let s = renderString . layoutCompact $ pretty e
      pure $ s ++ ", "
    pure $ "[ " ++ join ss ++ " ]\n"
  pure $ join p

indexBoard :: [[a]] -> [[(Position, a)]]
indexBoard elements =
  let indexedX = fmap indexed elements
      indexedY = indexed indexedX
   in fmap (\(x, r) -> (\(y, e) -> (Position (Width x) (Height y), e)) <$> r) indexedY

printMap :: [Position] -> String
printMap positions =
  let sortedX = sortOn (\x -> x.x) positions
      sortedY = sortOn (\x -> x.y) positions
      minX = traceShowId $ (head sortedX).x
      minY = traceShowId $ (head sortedY).y
      maxX = traceShowId $ (last sortedX).x
      maxY = traceShowId $ (last sortedY).y
      isIn a = a `elem` positions
   in do
        y <- [minY - 1 .. maxY + 1]
        x <- [minX - 1 .. maxX + 2]
        pure $
          if x == maxX + 2
            then '\n'
            else
              if isIn (Position x y)
                then '#'
                else '.'
