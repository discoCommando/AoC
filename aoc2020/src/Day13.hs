module Day13 where

import qualified Board
import Common
import Control.Lens
import Data.Data
import Data.List (sortBy, sortOn)
import Data.Maybe (catMaybes)
import qualified Debug.Trace
import Parseable
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

data Input = Input
  { time :: Integer,
    busIds :: [Maybe Integer]
  }
  deriving stock (Generic)

inputParser :: Parser Input
inputParser = do
  time <- Mega.decimal
  _ <- Mega.newline
  busIds <-
    flip Mega.sepEndBy (Mega.char ',') $
      Mega.choice
        [ parseStringAs "x" Nothing,
          Just <$> Mega.decimal
        ]
  pure $ Input {..}

solution :: Solution Input Integer Integer
solution =
  Solution
    { parse = inputParser, -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

part1' :: Input -> Integer
part1' i =
  let busIds' = Debug.Trace.traceShowId $ sortOn (view _2) $ (\b -> (b, b - view #time i `mod` b)) <$> catMaybes (i ^. #busIds)
      (x, y) = head busIds'
   in x * y

data Rem = Rem
  { min :: Integer,
    rem :: Integer
  }
  deriving stock (Generic, Show)

part2' :: Input -> Integer
part2' i =
  let rems =
        Debug.Trace.traceShowId $ catMaybes $
          ( \(fromIntegral -> i', b) -> case b of
              Nothing -> Nothing
              Just x -> Just $ Rem x ((x - i') `mod` x)
          )
            <$> Board.indexed (i ^. #busIds)
      final = foldl1 part2Helper rems
   in final ^. #rem

part2Helper :: Rem -> Rem -> Rem
part2Helper r1 r2 =
  let mm = r1 ^. #min * r2 ^. #min
      rr = if r1 ^. #min < r2 ^. #min then r2 else r1
      mm2 = mm - rr ^. #min + rr ^. #rem
      a = Debug.Trace.traceShowId $ head [x | x <- [mm2, mm2 - rr ^. #min .. 0], x `mod` view #min r1 == view #rem r1, x `mod` view #min r2 == view #rem r2]
   in Rem mm a

main =
  aoc
    "13"
    solution
