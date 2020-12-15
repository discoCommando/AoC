module Day11 where

import Board
import Common
import Control.Lens hiding (Empty)
import Control.Monad (join, when)
import Control.Monad.ST
import Data.Data
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import Data.Text.Prettyprint.Doc
import Data.Traversable (for)
import qualified Debug.Trace
import Parseable
import STState
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data Tile = Floor | TSeat Seat deriving stock (Show, Eq, Generic)

instance Pretty Tile where
  pretty = \case
    Floor -> "."
    TSeat s -> pretty s

data Seat = Occupied | Empty deriving stock (Show, Eq, Generic)

instance Pretty Seat where
  pretty = \case
    Occupied -> "#"
    Empty -> "L"

inputParser :: Parser [Tile]
inputParser = Mega.some $ Mega.choice [parseStringAs "." Floor, parseStringAs "L" $ TSeat Empty, parseStringAs "#" $ TSeat Occupied]

solution :: Solution [[Tile]] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

data Cell = Cell {turn :: Int, tile :: Tile, previousTile :: Tile}
  deriving stock (Generic, Show, Eq)

instance Pretty Cell where
  pretty Cell {..} = if previousTile == tile then pretty tile else pretty previousTile <+> " -> " <+> pretty tile

data State1 s = State1
  { board :: STBoard s Cell,
    turn :: Int,
    same :: Bool
  }
  deriving (Generic)

part1' :: [[Tile]] -> Int
part1' tiles = runST $ do
  board <- newBoard $ fmap (\a -> Cell 0 a a) <$> tiles
  let initState =
        State1
          { board = board,
            turn = 0,
            same = False
          }
  (v, _) <- initSTState initState part1Helper
  pure v

part1Helper :: STState (State1 s) s Int
part1Helper = do
  turn <- uses #turn (+ 1)
  #turn .= turn
  #same .= True
  board <- use #board
  indexedMMap board $ \cell pos -> do
    let tile = cell ^. #tile
    newTile <- case tile of
      Floor -> pure Floor
      TSeat Empty -> do
        neighbours <- getPossiblePositions pos WithDiagonals board
        let occupying = sum $ flip fmap neighbours $ \(_, cell') ->
              let tile' =
                    if turn == cell' ^. #turn
                      then cell' ^. #previousTile
                      else cell' ^. #tile
               in if tile' == TSeat Occupied
                    then 1
                    else 0
        pure $ if occupying == 0 then TSeat Occupied else TSeat Empty
      TSeat Occupied -> do
        neighbours <- getPossiblePositions pos WithDiagonals board
        let occupying = sum $ flip fmap neighbours $ \(_, cell') ->
              let tile' =
                    if turn == cell' ^. #turn
                      then cell' ^. #previousTile
                      else cell' ^. #tile
               in if tile' == TSeat Occupied
                    then 1
                    else 0
        pure $ if occupying >= 4 then TSeat Empty else TSeat Occupied
    when (newTile /= tile) $ do
      #same .= False
    cell & #previousTile .~ tile
      & #tile .~ newTile
      & #turn .~ turn
      & pure
  same <- use #same
  if same
    then foldM board 0 $ \e _p ->
      pure
        . ( +
              case e ^. #tile of
                TSeat Occupied -> 1
                _ -> 0
          )
    else do
      part1Helper

data Cell2 = Cell2
  { seat :: Seat,
    previousSeat :: Seat,
    turn :: Int,
    neighbours :: Set.Set Position
  }
  deriving stock (Generic)

data State2 = State2
  { board :: Map.Map Position Cell2,
    turn :: Int,
    same :: Bool
  }
  deriving stock (Generic)

markNeighbours :: Width -> Height -> Position -> Position -> Maybe Position -> Map.Map Position Cell2 -> Map.Map Position Cell2
markNeighbours mwidth mheight vector currentPosition currentTile map =
  if currentPosition ^. #x >= mwidth
    || currentPosition ^. #y >= mheight
    || currentPosition ^. #x < 0
    || currentPosition ^. #y < 0
    then map
    else
      let c = map Map.!? currentPosition
          (currentTile', map') = case c of
            Nothing -> (currentTile, map)
            Just _cell -> case currentTile of
              Nothing -> (Just currentPosition, map)
              Just p ->
                ( Just currentPosition,
                  map
                    & Map.update (Just . (#neighbours %~ Set.insert p)) currentPosition
                    & Map.update (Just . (#neighbours %~ Set.insert currentPosition)) p
                )
       in markNeighbours mwidth mheight vector (currentPosition + vector) currentTile' map'

initMarkNeighbours :: Width -> Height -> Position -> Position -> Map.Map Position Cell2 -> Map.Map Position Cell2
initMarkNeighbours w h v pos m =
  markNeighbours
    w
    h
    v
    pos
    Nothing
    m

initState :: [[Tile]] -> State2
initState tiles =
  let width = Width . length $ head tiles
      height = Height $ length tiles
      initBoard =
        Map.fromList . catMaybes $
          ( \(y, l) ->
              ( \(x, t) -> case t of
                  TSeat s -> Just (Position (Width x) (Height y), Cell2 s s 0 Set.empty)
                  _ -> Nothing
              )
                <$> Board.indexed l
          )
            =<< Board.indexed tiles
      directions =
        [ (Position 0 1, flip Position 0 <$> [0 .. width - 1]),
          (Position 1 0, Position 0 <$> [0 .. height - 1]),
          (Position 1 1, (Position 0 <$> [0 .. height - 1]) <> (flip Position 0 <$> [0 .. width - 1])),
          (Position 1 (-1), (Position 0 <$> [0 .. height - 1]) <> (flip Position (height - 1) <$> [0 .. width - 1]))
        ]
      updatedBoard =
        Prelude.foldl (\m (v, ps) -> Prelude.foldl (flip $ initMarkNeighbours width height v) m ps) initBoard directions
   in State2
        { board = updatedBoard,
          turn = 0,
          same = False
        }

part2' :: [[Tile]] -> Int
part2' tiles = runST $ do
  let initState' = initState tiles
  (v, _) <- initSTState initState' part2Helper
  pure v

part2Helper :: STState State2 s Int
part2Helper = do
  turn <- uses #turn (+ 1)
  #turn .= turn
  #same .= True
  board <- use #board
  newBoard <- flip Map.traverseWithKey board $ \_ cell -> do
    let seat = cell ^. #seat
    let neighbours = cell ^. #neighbours
    let occupying =
          Set.foldl
            ( \a p ->
                let s' = board Map.! p
                 in case s' ^. #seat of
                      Occupied -> a + 1
                      _ -> a
            )
            0
            neighbours
    let newSeat = case seat of
          Empty -> if occupying == 0 then Occupied else Empty
          Occupied -> if Set.size neighbours >= 5 && occupying >= 5 then Empty else Occupied
    when (seat /= newSeat) $ do
      #same .= False
    cell & #seat .~ newSeat & pure
  same <- use #same
  #board .= newBoard
  if same
    then pure $ Map.foldl (\a s -> if s ^. #seat == Occupied then a + 1 else a) 0 board
    else part2Helper

main =
  aoc
    "11"
    solution
