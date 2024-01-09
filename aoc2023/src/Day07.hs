module Day07 where

import Common
import Data.Data
import Data.List (nub, sort)
import GHC.OldList (sortOn)
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data InputLine h = InputLine
  { hand :: h,
    power :: Int
  }
  deriving stock (Generic, Show, Eq)

instance (Ord h) => Ord (InputLine h) where
  compare i1 i2 = compare i1.hand i2.hand

inputsP1 :: String
inputsP1 = "AKQJT98765432"

inputsP2 :: String
inputsP2 = "AKQT98765432J"

data Combo
  = HighCard String
  | Pair Char String
  | TwoPair Char Char Char
  | ThreeOfAKind Char String
  | FullHouse Char Char
  | FourOfAKind Char Char
  | FiveOfAKind Char
  deriving stock (Generic, Show, Eq)

data Hand = Hand
  { combo :: Combo,
    cards :: String
  }
  deriving stock (Generic, Show, Eq)

data Hand2 = Hand2
  { combo2 :: Combo,
    cards2 :: String
  }
  deriving stock (Generic, Show, Eq)

instance Ord Combo where
  compare d1 d2 =
    case d1 of
      FiveOfAKind _ ->
        case d2 of
          FiveOfAKind _ -> EQ
          _ -> GT
      FourOfAKind _ _ ->
        case d2 of
          FiveOfAKind _ -> LT
          FourOfAKind _ _ -> EQ
          _ -> GT
      FullHouse _ _ ->
        case d2 of
          FiveOfAKind _ -> LT
          FourOfAKind _ _ -> LT
          FullHouse _ _ -> EQ
          _ -> GT
      ThreeOfAKind _ _ ->
        case d2 of
          FiveOfAKind _ -> LT
          FourOfAKind _ _ -> LT
          FullHouse _ _ -> LT
          ThreeOfAKind _ _ -> EQ
          _ -> GT
      TwoPair _ _ _ ->
        case d2 of
          FiveOfAKind _ -> LT
          FourOfAKind _ _ -> LT
          FullHouse _ _ -> LT
          ThreeOfAKind _ _ -> LT
          TwoPair _ _ _ -> EQ
          _ -> GT
      Pair _ _ ->
        case d2 of
          FiveOfAKind _ -> LT
          FourOfAKind _ _ -> LT
          FullHouse _ _ -> LT
          ThreeOfAKind _ _ -> LT
          TwoPair _ _ _ -> LT
          Pair _ _ -> EQ
          _ -> GT
      HighCard _ ->
        case d2 of
          FiveOfAKind _ -> LT
          FourOfAKind _ _ -> LT
          FullHouse _ _ -> LT
          ThreeOfAKind _ _ -> LT
          TwoPair _ _ _ -> LT
          Pair _ _ -> LT
          HighCard _ -> EQ

powerOf :: String -> Char -> Int
powerOf inputs c = snd $ head $ filter ((== c) . fst) powers
  where
    powers = zip (reverse $ inputs) [1 ..]

instance Ord Hand where
  compare h1 h2 =
    case compare h1.combo h2.combo of
      EQ ->
        compare (powerOf inputsP1 <$> h1.cards) (powerOf inputsP1 <$> h2.cards)
      x -> x

instance Ord Hand2 where
  compare h1 h2 =
    case compare h1.combo2 h2.combo2 of
      EQ ->
        compare (powerOf inputsP2 <$> h1.cards2) (powerOf inputsP2 <$> h2.cards2)
      x -> x

getCombo :: String -> Combo
getCombo cards =
  let cardCounts = sortOn ((* (-1)) . snd) $ nub $ getCount cards <$> cards
   in case cardCounts of
        [(c, 5)] -> FiveOfAKind c
        [(c1, 4), (c2, 1)] -> FourOfAKind c1 c2
        [(c1, 3), (c2, 2)] -> FullHouse c1 c2
        [(c1, 3), (c2, 1), (c3, 1)] -> ThreeOfAKind c1 (c2 : [c3])
        [(c1, 2), (c2, 2), (c3, 1)] -> TwoPair c1 c2 c3
        [(c1, 2), (c2, 1), (c3, 1), (c4, 1)] -> Pair c1 (c2 : [c3, c4])
        [(c1, 1), (c2, 1), (c3, 1), (c4, 1), (c5, 1)] -> HighCard (c1 : [c2, c3, c4, c5])
        _ -> undefined

getCount :: String -> Char -> (Char, Int)
getCount cards c = (c,) . length $ filter (== c) cards

getComboP2 :: String -> Hand2
getComboP2 cards =
  let js = snd $ getCount cards 'J'
   in case js of
        0 -> flip Hand2 cards $ getCombo cards
        5 -> flip Hand2 cards $ FiveOfAKind 'J'
        _ ->
          let otherCards = filter (/= 'J') cards
              newCards c = map (\c' -> if c' == 'J' then c else c') cards
              newHand c = flip Hand2 cards (getCombo $ newCards c)
              otherHands = reverse $ sort $ newHand <$> otherCards
           in head otherHands

pHand :: Parser Hand
pHand = do
  cards <- Mega.some Mega.alphaNumChar
  pure $ Hand (getCombo cards) cards

inputParser :: Parser (InputLine Hand)
inputParser = do
  hand <- pHand
  spaces
  power <- Mega.decimal
  pure $ InputLine {..}

solution :: Solution [(InputLine Hand)] Int Int
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

part1' :: [(InputLine Hand)] -> Int
part1' = sum . fmap (uncurry (*)) . zip [1 ..] . fmap (.power) . sort

part2' :: [(InputLine Hand)] -> Int
part2' =
  sum
    . fmap (uncurry (*))
    . zip [1 ..]
    . fmap (.power)
    . logMe' "comboed"
    . sort
    . fmap
      ( \i ->
          i {hand = getComboP2 i.hand.cards}
      )

main =
  aoc
    "07"
    solution
