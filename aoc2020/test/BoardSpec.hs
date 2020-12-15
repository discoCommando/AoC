module BoardSpec where

import Board
import Control.Lens ((&))
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Traversable (for)
import Test.Hspec

spec =
  describe "all" $ do
    forM_ (indexed l) $ \(y, l') ->
      forM_ (indexed l') $ \(x, i) ->
        it (show x <> " " <> show y) $ do
          v <- stToIO $ do
            board <- testBoard
            let pos = Position (Width x) (Height y)
            v1 <- unsafeGet pos board
            unsafeSet pos (i + 10) board
            v2 <- unsafeGet pos board
            pure (v1, v2)
          v `shouldBe` (i, i + 10)
    it "aaa" $ do
      v <- stToIO $ do
        board <- testBoard
        n1 <- getPossiblePositions (Position 0 0) WithDiagonals board
        n2 <- getPossiblePositions (Position 0 0) WithoutDiagonals board
        pure (n1, n2)
      v `shouldBe` ([(Position 0 1, 2), (Position 1 0, 2), (Position 1 1, 3)], [(Position 0 1, 2), (Position 1 0, 2)])
      v2 <- stToIO $ do
        board <- testBoard
        n1 <- getPossiblePositions (Position 2 2) WithDiagonals board
        n2 <- getPossiblePositions (Position 2 2) WithoutDiagonals board
        pure (n1, n2)
      v2 `shouldBe` ([(Position 1 1, 3), (Position 1 2, 6), (Position 2 1, 4)], [(Position 2 1, 4), (Position 1 2, 6)])

l = [[1, 2, 3], [2, 3, 4], [5, 6, 8]]

testBoard :: ST s (STBoard s Int)
testBoard =
  newBoard l
