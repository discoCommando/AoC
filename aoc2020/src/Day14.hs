module Day14 where

import qualified Board
import Common
import Control.Lens
import Control.Monad (join)
import qualified Control.Monad.State as State
import Data.Bits
import Data.Data
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Debug.Trace
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data InputLine = Mem {address :: Integer, value :: Integer} | Mask [Maybe Bool] deriving stock (Generic, Show, Eq)

inputParser :: Parser InputLine
inputParser =
  Mega.choice
    [ do
        Mega.chunk "mem["
        address <- Mega.decimal
        Mega.chunk "] = "
        value <- Mega.decimal
        pure Mem {..},
      do
        Mega.chunk "mask = "
        Mask
          <$> Mega.some
            ( Mega.choice
                [ parseStringAs "X" Nothing,
                  parseStringAs "1" $ Just True,
                  parseStringAs "0" $ Just False
                ]
            )
    ]

solution :: Solution [InputLine] Integer Integer
solution =
  Solution
    { parse = Mega.sepEndBy inputParser Mega.newline, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

data MaskV = MaskV
  { position :: Int,
    isOne :: Bool
  }
  deriving stock (Generic, Show)

data State1 = State1
  { mask :: [MaskV],
    memory :: Map.Map Integer Integer
  }
  deriving stock (Generic)

part1' :: [InputLine] -> Integer
part1' ls = do
  let initState =
        State1
          { mask = [],
            memory = Map.empty
          }
  let s = flip State.execState initState $ traverse part1Helper $ Debug.Trace.traceShowId ls
  sum . fmap (view _2) . Map.toList $ s ^. #memory

part1Helper :: InputLine -> State.State State1 ()
part1Helper (Mask ls) = do
  let v = uncurry MaskV <$> (mapMaybe (\(i, x) -> (i,) <$> x) . Board.indexed . reverse $ ls)
  #mask .= v
part1Helper (Mem address value) = do
  mask <- use #mask
  #memory %= Map.insert address (foldl (flip convert) value mask)
  pure ()

convert :: MaskV -> Integer -> Integer
convert mv =
  if mv ^. #isOne
    then flip setBit $ mv ^. #position
    else flip clearBit $ mv ^. #position

data State2 = State2
  { mask :: [(Int, Maybe Bool)],
    memory :: Map.Map Integer Integer
  }
  deriving (Generic)

part2' :: [InputLine] -> Integer
part2' ls = do
  let initState =
        State2
          { mask = [],
            memory = Map.empty
          }
  let s = flip State.execState initState $ traverse part2Helper $ Debug.Trace.traceShowId ls
  sum . fmap (view _2) . Map.toList $ s ^. #memory

part2Helper :: InputLine -> State.State State2 ()
part2Helper (Mask ls) = do
  #mask .= Board.indexed (reverse ls)
part2Helper (Mem address value) = do
  mask <- use #mask
  let addresses :: [Integer] = foldl (\acc (i, v) -> acc >>= convert' i v) [address] mask
  memory <- use #memory
  #memory .= foldl (\m a -> Map.insert a value m) memory addresses
  pure ()

convert' :: Int -> Maybe Bool -> Integer -> [Integer]
convert' ix v i =
  case v of
    Just False -> [i]
    Just True -> [i `setBit` ix]
    Nothing -> [i `setBit` ix, i `clearBit` ix]

main =
  aoc
    "14"
    solution
