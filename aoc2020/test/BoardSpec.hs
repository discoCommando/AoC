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

l = [[1, 2, 3], [2, 3, 4]]

testBoard :: ST s (STBoard s Int)
testBoard =
  newBoard l
