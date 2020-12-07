{-# LANGUAGE TypeFamilies #-}

module Day05 where

import Common
import Data.Data
import Data.Set
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data FB = F | B deriving stock (Show, Eq, Generic, Enum, Bounded)

data LR = L | R deriving stock (Show, Eq, Generic, Enum, Bounded)

instance KnownValue F where
  type Val F = FB
  valVal _ = F

instance KnownValue B where
  type Val B = FB
  valVal _ = B

instance KnownValue L where
  type Val L = LR
  valVal _ = L

instance KnownValue R where
  type Val R = LR
  valVal _ = R

type InputParser =
  SepEndBy
    ( Sized
        7
        ( Choice
            [ Const "F" F,
              Const "B" B
            ]
        )
        :+: Sized
              3
              ( Choice
                  [ Const "L" L,
                    Const "R" R
                  ]
              )
    )
    Newline

toNumber :: Enum a => [a] -> Int
toNumber =
  toNumberHelper 0 0 . reverse

toNumberHelper :: Enum a => Int -> Int -> [a] -> Int
toNumberHelper pow acc = \case
  [] -> acc
  x : rest -> toNumberHelper (pow + 1) (fromEnum x * 2 ^ pow + acc) rest

part1' :: Ret InputParser -> Int
part1' = maximum . fmap hash

hash :: ([FB], [LR]) -> Int
hash (a, b) = toNumber a * 8 + toNumber b

strip :: Eq a => [a] -> [a] -> [a]
strip (x : xs) (y : ys) = if x == y then strip xs ys else y : ys

allNumbers :: Set Int
allNumbers = Data.Set.fromAscList [hash ([F, F, F, F, F, F, F], [L, L, L]) .. hash ([B, B, B, B, B, B, B], [R, R, R])]

part2' :: Ret InputParser -> Int
part2' = head . strip (reverse [0 .. 1023]) . reverse . strip [0 ..] . Data.Set.toAscList . Prelude.foldl (\acc n -> Data.Set.delete (hash n) acc) allNumbers

solution =
  Solution
    { parse = parser (Proxy :: Proxy InputParser), -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

main =
  aoc
    "05"
    solution
