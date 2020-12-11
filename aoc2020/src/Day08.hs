{-# LANGUAGE TypeFamilies #-}

module Day08 where

import Board (indexed)
import Common
import qualified Control.Applicative
import Control.Lens hiding (Choice, Const, indexed)
import Control.Monad.ST
import Data.Array.ST
import Data.Data
import qualified Data.Map.Strict as Data.Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Sequence
import qualified Data.Set
import Parseable
import STState
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

-- type Int' =
--   Choice
--     [ Const "+" True,
--       Const "-" False
--     ]
--     :+: ToInt (Some Digit)

-- data Operation = NoOperation | Accumulate | Jump
--   deriving stock (Generic, Show, Eq)

-- instance KnownValue NoOperation where
--   type Val NoOperation = Operation
--   valVal _ = NoOperation

-- instance KnownValue Accumulate where
--   type Val Accumulate = Operation
--   valVal _ = Accumulate

-- instance KnownValue Jump where
--   type Val Jump = Operation
--   valVal _ = Jump

-- type InputLine' = Choice [Const "nop" NoOperation, Const "acc" Accumulate, Const "jmp" Jump] <$ Space :+: Int'

-- data InputLine = InputLine {operation :: Operation, val :: Int}
--   deriving stock (Generic, Eq, Show)

-- conv :: Ret InputLine' -> InputLine
-- conv a = InputLine (a ^. _1) ((if view (_2 . _1) a then 1 else (-1)) * view (_2 . _2) a)

data Operation = NoOperation Position | Accumulate Accumulator | Jump Position
  deriving stock (Generic, Show, Eq)

intParser :: Parser Operation
intParser = do
  f <- Mega.choice [parseStringAs "nop" (NoOperation . Position), parseStringAs "acc" (Accumulate . Accumulator), parseStringAs "jmp" (Jump . Position)]
  _ <- Mega.space1
  m <- Mega.choice [parseStringAs "-" (-1), parseStringAs "+" 1]
  v <- Mega.decimal
  pure $ f (m * v)

solution =
  Solution
    { parse = Mega.sepEndBy intParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

newtype Position = Position {getPosition :: Int}
  deriving newtype (Show, Eq, Ord, Ix, Num)
  deriving stock (Generic)

newtype Accumulator = Accumulator {getAccumulator :: Int}
  deriving newtype (Show, Eq, Ord, Num)
  deriving stock (Generic)

data Cell1 = Cell1
  { operation :: Operation,
    visited :: Bool
  }
  deriving stock (Generic, Show, Eq)

newtype State1 = State1
  { operations :: Data.Map.Map Position Cell1
  }
  deriving stock (Generic, Show, Eq)

part1' ops = runST $ do
  let initialState =
        State1
          { operations = Data.Map.fromList $ (_2 %~ flip Cell1 False) . (_1 %~ Position) <$> indexed ops
          }
  (v, _) <- initSTState initialState $ part1Helper 0 0
  pure v

execute :: Operation -> Position -> Accumulator -> (Position, Accumulator)
execute operation pos acc =
  case operation of
    NoOperation _ -> (pos + 1, acc)
    Accumulate newAcc -> (pos + 1, acc + newAcc)
    Jump newPos -> (pos + newPos, acc)

part1Helper :: Position -> Accumulator -> STState State1 s Accumulator
part1Helper pos acc = do
  cell <- uses #operations (Data.Map.! pos)
  if cell ^. #visited
    then pure acc
    else do
      #operations %= Data.Map.update (Just . (#visited .~ True)) pos
      uncurry part1Helper $ execute (cell ^. #operation) pos acc

data Cell2 = Cell2
  { visited :: Data.Set.Set Position,
    pos :: Position,
    acc :: Accumulator,
    override :: Data.Map.Map Position Operation
  }
  deriving (Generic)

data State2 s = State2
  { ops :: STArray s Position Operation,
    queue :: Data.Sequence.Seq Cell2
  }
  deriving (Generic)

part2' :: [Operation] -> Int
part2' ls = runST $ do
  ops <- newListArray (0, Position (length ls) - 1) ls
  let overrides =
        mapMaybe
          ( \(i, o) -> case o of
              NoOperation x -> Just $ Data.Map.singleton (Position i) $ Jump x
              Jump x -> Just $ Data.Map.singleton (Position i) $ NoOperation x
              _ -> Nothing
          )
          $ indexed ls
  let queue = Data.Sequence.fromList $ Cell2 Data.Set.empty 0 0 <$> overrides
  view (_1 . #getAccumulator) <$> initSTState (State2 ops queue) part2Helper

part2Helper :: STState (State2 s) s Accumulator
part2Helper = do
  queue <- use #queue
  let (cell Data.Sequence.:< rest) = Data.Sequence.viewl queue
  #queue .= rest
  if Data.Set.member (cell ^. #pos) $ cell ^. #visited
    then part2Helper
    else do
      ops :: STArray s Position Operation <- use #ops
      range <- liftS $ getBounds ops
      if not $ inRange range (cell ^. #pos)
        then pure $ cell ^. #acc
        else do
          opFromArr <- liftS $ readArray ops (cell ^. #pos)
          let opFromOverride = Data.Map.lookup (cell ^. #pos) (cell ^. #override)
          let operation = fromMaybe opFromArr opFromOverride
          let (newPos, newAcc) = execute operation (cell ^. #pos) (cell ^. #acc)
          let newCell =
                cell
                  & #pos .~ newPos
                  & #acc .~ newAcc
                  & #visited %~ Data.Set.insert (cell ^. #pos)
          #queue %= (Data.Sequence.|> newCell)
          part2Helper

main =
  aoc
    "08"
    solution
