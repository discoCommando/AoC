module BinSearchSpec where

import BinSearch
import Control.Applicative ((<|>))
import Control.Monad (forM, forM_)
import Data.List
import Test.Hspec
import Test.QuickCheck

data SortedUnique = SortedUnique [Int]
  deriving stock (Show, Ord, Eq)

instance Arbitrary SortedUnique where
  arbitrary = do
    ints :: [Int] <- arbitrary
    pure $ SortedUnique $ sort $ nub ints

binSearchSortedUnique :: Int -> SortedUnique -> Maybe Int
binSearchSortedUnique searchedValue (SortedUnique su) =
  binSearchEq (length su) (getter su) searchedValue

getter :: [Int] -> Int -> Int
getter [] _ = undefined
getter (x : xs) i = if i == 0 then x else getter xs (i - 1)

binSearchBiggestUntilSortedUnique :: Int -> SortedUnique -> Maybe (Int, Int)
binSearchBiggestUntilSortedUnique searchedValue (SortedUnique su) =
  binSearchBiggestUntil (length su) (getter su) searchedValue

spec =
  fdescribe "binary search tests" $ do
    it "binary search" $ do
      property $ \(SortedUnique su) -> forM_ [0 .. length su - 1] $ \i -> do
        let v = getter su i
         in binSearchSortedUnique v (SortedUnique su) `shouldBe` Just i
    it "binary search for smallest element bigger than x" $ do
      property $ \(SortedUnique su) -> forM_ [0 .. length su - 1] $ \i -> do
        let v = getter su i
            findSmallestIndex id' = \case
              [] -> Nothing
              (v' : rest) ->
                if v' < v
                  then findSmallestIndex (id' + 1) rest <|> Just (id', v')
                  else Nothing
         in binSearchBiggestUntilSortedUnique v (SortedUnique su) `shouldBe` findSmallestIndex 0 su