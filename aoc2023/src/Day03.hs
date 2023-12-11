module Day03 where

import Board (Position (..))
import Common
import Control.Monad (guard)
import Data.Data
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega
import Text.Read (readMaybe)

data S = Dot | Digit Int | Symbol Char
  deriving stock (Show, Eq, Generic)

pS :: Parser S
pS =
  Mega.choice
    [ Dot <$ Mega.char '.',
      Digit . unsafeMaybe . readMaybe . pure
        <$> Mega.digitChar,
      (log' "symbol" "" . Symbol) <$> (Mega.printChar <|> Mega.symbolChar)
    ]

data InputLine = InputLine
  { line :: [S]
  }
  deriving stock (Generic, Show, Eq)

inputParser :: Parser InputLine
inputParser = InputLine <$> Mega.many pS

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

data Cell = Sym Char | Num Int
  deriving stock (Generic, Show, Eq)

data CellPos = CellPos
  { cell :: Cell,
    pos :: [Position]
  }
  deriving stock (Generic, Show, Eq)

data Board = Board
  { board :: [CellPos]
  }
  deriving stock (Generic, Show, Eq)

data Op = A CellPos | H CellPos

helper :: S -> Position -> Maybe (Int, [Position]) -> ([CellPos], Maybe (Int, [Position]))
helper s p mCellPos =
  case mCellPos of
    Nothing ->
      case s of
        Dot -> ([], Nothing)
        Symbol c -> (pure $ CellPos (Sym c) [p], Nothing)
        Digit i -> ([], Just $ (i, [p]))
    Just (i, pos) ->
      case s of
        Dot -> (pure $ CellPos (Num i) pos, Nothing)
        Symbol c -> ([CellPos (Sym c) [p], CellPos (Num i) pos], Nothing)
        Digit i' ->
          if (p.y /= (head pos).y)
            then -- this is the case where we have an entry from a new line
              ([CellPos (Num i) pos], Just $ (i', [p]))
            else ([], Just $ (i * 10 + i', p : pos))

toBoard' :: [InputLine] -> Board
toBoard' inputLines =
  Board
    { board =
        step2
          $ logEmpty'
            "symbols before"
            ( length
                . filter
                  ( \(_, s) -> case s of
                      Symbol _ -> True
                      _ -> False
                  )
            )
          $ step1 inputLines
    }
  where
    step1 :: [InputLine] -> [(Position, S)]
    step1 inputLines' =
      [ (Position x y, s)
        | (y, line) <- zip [0 ..] $ (.line) <$> inputLines',
          (x, s) <- zip [0 ..] line
      ]
    mAdd :: Maybe (Int, [Position]) -> [CellPos] -> [CellPos]
    mAdd op acc = case op of
      Nothing -> acc
      Just (i, pos) -> (CellPos (Num i) pos) : acc
    step2 :: [(Position, S)] -> [CellPos]
    step2 l =
      let (acc, x) =
            foldl
              ( \(acc, op) (p, s) ->
                  let (toAdd, newAcc) = helper s p op
                   in (acc ++ toAdd, newAcc)
              )
              ([], Nothing)
              l
       in mAdd x acc
            ++ log'
              "symbols after"
              ( length
                  $ filter
                    ( \case
                        Sym _ -> True
                        _ -> False
                    )
                  $ fmap (.cell) (mAdd x acc)
              )
              []

-- first cellpos is a number
isAdjacent :: CellPos -> CellPos -> Bool
isAdjacent a@CellPos {pos = p1} b@CellPos {pos = p2, cell = cell} =
  if a == b
    then False
    else case cell of
      Num _ -> False
      _ -> or [posIsAdjacent p1' p2' | p1' <- p1, p2' <- p2]

-- first cellpos is a symbol
isAdjacentNumber :: CellPos -> CellPos -> Maybe Int
isAdjacentNumber a@CellPos {pos = p1} b@CellPos {pos = p2, cell = cell2} =
  if a == b
    then Nothing
    else case cell2 of
      Sym _ -> Nothing
      Num i -> if or [posIsAdjacent p1' p2' | p1' <- p1, p2' <- p2] then Just i else Nothing

posIsAdjacent :: Position -> Position -> Bool
posIsAdjacent (Position x1 y1) (Position x2 y2) =
  (abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1) && (x1 /= x2 || y1 /= y2)

part1' :: [InputLine] -> Int
part1' inputs =
  let board = toBoard' inputs
      nums =
        mapMaybe
          ( \x -> case x.cell of
              Num _ -> Just x
              _ -> Nothing
          )
          board.board
   in sum $ log' "result before sum" "" $ do
        a <- nums
        guard $ or [isAdjacent a b | b <- board.board]
        pure $ toI a
  where
    toI :: CellPos -> Int
    toI CellPos {cell = Num i} = i
    toI _ = 0

part2' :: [InputLine] -> Int
part2' inputs =
  let board = toBoard' inputs
      symbols =
        mapMaybe
          ( \x -> case x.cell of
              Sym '*' -> Just x
              _ -> Nothing
          )
          board.board
   in sum $ log' "result before sum" "" $ do
        a <- symbols
        let nums = mapMaybe (isAdjacentNumber a) board.board
        if length nums /= 2
          then []
          else do
            pure $ product nums

main =
  aoc
    "03"
    solution
