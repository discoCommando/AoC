{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- {-# LANGUAGE UndecidableInstances #-}

-- taken from https://github.com/blinry/advent-of-code-2019/blob/master/Common.hs

module Common
  ( module X,
    Solution (..),
    benchmark,
    aoc,
    tbd,
    toInt,
    listParser,
    takeWhen,
    unsafeMaybe,
    unsafeParseExample,
    Parser,
    unsafeParse,
    parseStringAs,
    tryOneOf,
    unsafeGet',
    PrimitiveBoard,
    Point,
    getAtBoard,
    log',
    logEmpty',
    spaces,
    logMe',
  )
where

import Control.Applicative as X (Alternative ((<|>)))
import Control.Exception
import qualified Control.Lens as X
import Data.Functor
import Data.Generics.Labels ()
import Data.Generics.Product as X hiding (IsList, list)
import Data.Generics.Sum as X
import Data.Void
import Debug.Trace as X
import Formatting
import Formatting.Clock
import GHC.Base (Alternative ((<|>)))
import GHC.Exts as X (IsList)
import GHC.Generics as X (Generic)
import System.Clock
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega

data Solution a b c = Solution
  { parse :: Parser a,
    part1 :: a -> b,
    part2 :: a -> c
  }

benchmark :: IO a -> IO a
benchmark action = do
  start <- getTime Monotonic
  a <- action
  end <- getTime Monotonic
  fprint (" (" % timeSpecs % ")\n") start end
  pure a

aoc :: (Show b, Show c) => String -> Solution a b c -> IO ()
aoc n solution = do
  let fileName = "./data/" ++ n ++ ".txt"
  input <- readFile fileName
  let problem' = Mega.parse (parse solution) "input" input
  problem <- benchmark $ do
    putStr "Parsing input..."
    a <- evaluate problem'
    case a of
      Left e -> do
        putStrLn $ Mega.errorBundlePretty e
        throwIO e
      Right a' -> pure a'
  benchmark $ putStr $ "Part 1: " ++ show (part1 solution problem)
  benchmark $ putStr $ "Part 2: " ++ show (part2 solution problem)

tbd :: a -> String
tbd _x = "(not implemented)"

-- freqs :: Ord a => [a] -> Map a Integer
-- freqs = Map.fromListWith (+) . map (,1)

toInt :: String -> Int
toInt = read

type Parser a = Mega.Parsec Void String a

listParser :: (Show a) => Parser delimiter -> Parser a -> Parser [a]
listParser s p =
  Mega.sepEndBy p s

takeWhen :: (Show a) => Parser a -> Parser [a]
takeWhen =
  Mega.many

unsafeMaybe :: Maybe a -> a
unsafeMaybe = \case
  Nothing -> undefined
  Just a -> a

unsafeParse :: Parser a -> String -> a
unsafeParse p = unsafeMaybe . Mega.parseMaybe p

unsafeParseExample :: Solution a b c -> String -> a
unsafeParseExample s =
  unsafeMaybe . Mega.parseMaybe (parse s)

unsafeGet' :: [a] -> Int -> a
unsafeGet' [] _ = undefined
unsafeGet' (x : xs) l = if l == 0 then x else unsafeGet' xs (l - 1)

type PrimitiveBoard a = [[a]]

type Point = (Int, Int)

getAtBoard :: Point -> PrimitiveBoard a -> Maybe a
getAtBoard (x, y) b
  | x < 0 || x >= length (head b) = Nothing
  | y < 0 || y >= length b = Nothing
  | otherwise = Just $ b !! y !! x

parseStringAs :: String -> a -> Parser a
parseStringAs s a =
  Mega.chunk s >> pure a

tryOneOf :: [Parser a] -> Parser a
tryOneOf [] = Mega.empty
tryOneOf [x] = x
tryOneOf (x : xs) = Mega.try x <|> tryOneOf xs

log' :: (Show a, Show b) => String -> a -> b -> b
log' s a b = trace (s <> " for " <> show a <> "\n" <> s <> ": " <> show b) b

logEmpty' :: (Show a) => String -> (b -> a) -> b -> b
logEmpty' s f b = trace (s <> " for " <> show (f b)) b

logMe' :: (Show a) => a -> a
logMe' a = seq (trace (show a) a) a

spaces :: Parser ()
spaces = Mega.hspace

-- class Loggable a where
--   run :: a -> a

-- instance {-# OVERLAPPABLE #-} (Show a, Loggable b) => Loggable (a -> b) where
--   run :: forall a b. (Show a, Loggable b) => (a -> b) -> a -> b
--   run f x = traceShow x $ run @b $ f x

-- instance {-# OVERLAPPABLE #-} (Show a) => Loggable (a -> b) where
--   run :: forall a b. (Show a) => (a -> b) -> a -> b
--   run f x = traceShow x $ f x

-- -- instance {-# OVERLAPPABLE #-} (Show a) => Loggable a where
-- --   run a = traceShowId a

-- logF' :: (Loggable x) => String -> x -> x
-- logF' s x = trace s $ run x