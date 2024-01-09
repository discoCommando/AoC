module BinSearch where

import Common

binSearchGeneric ::
  forall result value.
  (Ord value) =>
  -- | first index
  Int ->
  -- | last index
  Int ->
  -- | getter
  (Int -> value) ->
  -- | generic function
  ( -- \| index
    Int ->
    -- \| current value
    value ->
    -- \| value to the left
    Maybe result ->
    -- \| value to the right
    Maybe result ->
    -- \| result
    Maybe result
  ) ->
  -- | result
  Maybe result
binSearchGeneric firstId lastId getter genericFunction =
  helper firstId lastId
  where
    helper :: Int -> Int -> Maybe result
    helper from to =
      if from > to
        then Nothing
        else
          let index = (to + from) `div` 2
           in genericFunction index (getter index) (helper from (index - 1)) (helper (index + 1) to)

-- | Finds the index of a searched value. Returns Nothing if it does not exist
binSearchEq ::
  (Ord value) =>
  -- | size
  Int ->
  -- | getter
  (Int -> value) ->
  -- | searched value
  value ->
  -- | result
  Maybe Int
binSearchEq size getter searchedValue =
  binSearchEq' 0 (size - 1) getter searchedValue

-- | Finds the index of a searched value. Returns Nothing if it does not exist
binSearchEq' ::
  (Ord value) =>
  -- | first index
  Int ->
  -- | last index
  Int ->
  -- | getter
  (Int -> value) ->
  -- | searched value
  value ->
  -- | result
  Maybe Int
binSearchEq' firstId lastId getter searchedValue =
  binSearchGeneric firstId lastId getter $ \index value left right ->
    case compare searchedValue value of
      LT -> left
      EQ -> Just index
      GT -> right

-- | Searches for the biggest value that is smaller than a searched value
binSearchBiggestUntil' ::
  forall value.
  (Ord value) =>
  -- | first id
  Int ->
  -- | last id
  Int ->
  -- | getter
  (Int -> value) ->
  -- | searched value
  value ->
  -- | result
  Maybe (Int, value)
binSearchBiggestUntil' firstId lastId getter searchedValue =
  binSearchGeneric firstId lastId getter $ \index value left right ->
    if searchedValue > value
      then right <|> Just (index, value)
      else left

-- | Searches for the biggest value that is smaller than a searched value
binSearchBiggestUntil ::
  forall value.
  (Ord value) =>
  -- | size
  Int ->
  -- | getter
  (Int -> value) ->
  -- | searched value
  value ->
  -- | result
  Maybe (Int, value)
binSearchBiggestUntil size getter searchedValue =
  binSearchBiggestUntil' 0 (size - 1) getter searchedValue

-- | Searches for the smallest value that is bigger than a searched value
binSearchSmallestUntil' ::
  forall value.
  (Ord value) =>
  -- | first id
  Int ->
  -- | last id
  Int ->
  -- | getter
  (Int -> value) ->
  -- | searched value
  value ->
  -- | result
  Maybe (Int, value)
binSearchSmallestUntil' firstId lastId getter searchedValue =
  binSearchGeneric firstId lastId getter $ \index value left right ->
    if searchedValue < value
      then left <|> Just (index, value)
      else right

-- | Searches for the smallest value that is bigger than a searched value
binSearchSmallestUntil ::
  forall value.
  (Ord value) =>
  -- | size
  Int ->
  -- | getter
  (Int -> value) ->
  -- | searched value
  value ->
  -- | result
  Maybe (Int, value)
binSearchSmallestUntil size getter searchedValue =
  binSearchSmallestUntil' 0 (size - 1) getter searchedValue