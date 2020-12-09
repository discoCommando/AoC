module Day07 where

import Common
import Control.Lens hiding (Choice, Const)
import Control.Monad (forM_)
import Control.Monad.ST
import Control.Monad.State (gets)
import Data.Data
import qualified Data.Map.Strict as Data.Map
import Data.Maybe (fromMaybe)
import Data.STRef
import Data.Traversable (for)
import qualified Debug.Trace
import Parseable
import STState
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Mega

type ColorDesc = Some NotWhitespace <$ Space

type Color = ColorDesc :+: ColorDesc

type ColorBag = Color <$ Choice [Chunk "bags", Chunk "bag"]

type IntColorBag = ToInt (Some Digit <$ Space) :+: ColorBag

type InputLine' =
  (ColorBag <$ Chunk " contain ")
    :+: Choice
          [ Const "no other bags" (EmptyArr IntColorBag),
            SepEndBy IntColorBag (Chunk ", ")
          ]
      <$ Chunk "."

data Bag = Bag
  { firstColor :: String,
    secondColor :: String
  }
  deriving stock (Generic, Eq, Show, Ord)

data InputLine = InputLine
  { base :: Bag,
    contains :: [(Int, Bag)]
  }
  deriving stock (Generic, Eq, Show)

toBag :: Ret ColorBag -> Bag
toBag b = Bag (b ^. _1) (b ^. _2)

toInputLine :: Ret InputLine' -> InputLine
toInputLine (b, r) =
  InputLine (toBag b) (fmap (_2 %~ toBag) r)

solution :: Solution [InputLine] Int Int
solution =
  Solution
    { parse = fmap toInputLine <$> parser (Proxy :: Proxy (SepEndBy InputLine' Newline)), -- No parsing required.
      part1 = part1',
      part2 = part2'
    }

part1' ls = runST $ part1Init ls

data State = State
  { hasBag :: Data.Map.Map Bag (Maybe Bool, [(Int, Bag)]),
    searchedBag :: Bag
  }
  deriving stock (Generic, Eq, Show)

part1Init :: [InputLine] -> ST s Int
part1Init ls = do
  stref <-
    newSTRef $
      State
        { hasBag = Data.Map.fromList $ fmap (\i -> (i ^. #base, (Nothing, i ^. #contains))) ls,
          searchedBag = Bag "shiny" "gold"
        }
  flip runSTState stref $ do
    forM_ ls $ part1Helper . view #base
  newState <- readSTRef stref
  pure
    . flip (-) 1 -- exclude itself
    . length
    . filter ((==) (Just True) . view (_2 . _1))
    . Data.Map.toList
    $ newState ^. #hasBag

part1Helper :: Bag -> STState State s Bool
part1Helper b = do
  searchedBag <- use #searchedBag
  if b == searchedBag
    then do
      #hasBag %= Data.Map.updateWithKey (\_ -> Just . (_1 ?~ True)) b
      pure True
    else do
      contents <- fromMaybe (Nothing, []) <$> uses #hasBag (Data.Map.lookup b)
      case contents ^. _1 of
        Just v -> pure v
        Nothing -> do
          res <- fmap or . for (contents ^. _2) $ part1Helper . view _2
          #hasBag %= Data.Map.insert b (contents & _1 ?~ res)
          pure res

newtype State2 = State2
  { counts :: Data.Map.Map Bag (Maybe Int, [(Int, Bag)])
  -- stack :: [Bag]
  }
  deriving stock (Generic, Eq, Show)

-- part2'

part2' :: [InputLine] -> Int
part2' ls = runST $ do
  let counts = Data.Map.fromList $ fmap (\i -> (i ^. #base, (Nothing, i ^. #contains))) ls
  (v, _) <- initSTState (State2 counts) (part2Helper $ Bag "shiny" "gold")
  pure v

part2Helper :: Bag -> STState State2 s Int
part2Helper b = do
  (mc, bs) <- fromMaybe (Nothing, []) <$> uses #counts (Data.Map.lookup b)
  case mc of
    Just x -> pure x
    Nothing -> do
      v <- fmap sum . for bs $ \(i, b') -> do
        v' <- part2Helper b'
        pure $ i * (v' + 1)
      #counts %= Data.Map.insert b (Just v, bs)
      pure v

main =
  aoc
    "07"
    solution
