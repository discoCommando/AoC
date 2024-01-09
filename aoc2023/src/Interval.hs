module Interval where

-- import Common (log', logEmpty')
import Data.List (sortOn)

-- | Single interval, start is always <= end, also for now working on Ints only
data SingleInterval = SingleInterval {start :: Integer, end :: Integer}
  deriving stock (Show, Eq)

-- | For now assuming that both ends are closed
data Interval = Interval
  { intervals ::
      -- this list is always sorted on start
      [SingleInterval]
  }
  deriving stock (Show, Eq)

-- | Joins two intervals together. Assumes that they are sorted on the "start"
-- Returns 'Nothing' if intervals are disjoint. Internal operation, use 'union' instead.
_joinSingleIntervals :: SingleInterval -> SingleInterval -> Maybe (SingleInterval)
_joinSingleIntervals i1 i2 =
  if i1.start > i2.start
    then undefined
    else -- x  x
    --      y  y

      if i1.end + 1 < i2.start
        then Nothing
        else Just $ SingleInterval i1.start (max i1.end i2.end)

mkSingleInterval :: Integer -> Integer -> SingleInterval
mkSingleInterval start end =
  if end < start
    then undefined
    else SingleInterval start end

-- | Assuming that it's closed both ends
singleton :: Integer -> Integer -> Interval
singleton start end =
  Interval $ pure $ mkSingleInterval start end

-- | Assuming that it's closed both ends
singleton' :: SingleInterval -> Interval
singleton' s =
  Interval $ pure $ s

fromList :: [(Integer, Integer)] -> Interval
fromList =
  fromList' . map (uncurry mkSingleInterval)

fromList' :: [SingleInterval] -> Interval
fromList' =
  union mempty -- using union to make sure that all single intervals are "merged"
    . Interval

toList :: Interval -> [(Integer, Integer)]
toList =
  map (\(SingleInterval s e) -> (s, e)) . (.intervals)

instance Semigroup Interval where
  (<>) = union

instance Monoid Interval where
  mempty = Interval mempty

-- | Internal function. Merges two lists of single intervals together.
-- Second list must be sorted on start
_merge :: [SingleInterval] -> [SingleInterval] -> [SingleInterval]
_merge acc [] = acc
_merge acc (i : []) = i : acc
_merge acc (i1 : i2 : is) =
  case _joinSingleIntervals i1 i2 of
    Nothing -> _merge (i1 : acc) (i2 : is)
    Just i -> _merge acc (i : is)

union :: Interval -> Interval -> Interval
union i1 i2 =
  Interval $ sortOn (.start) $ _merge [] $ sortOn (.start) (i1.intervals ++ i2.intervals)

-- | Intersects two intervals together.
-- Use internally only, use 'intersect' instead
_intersectSingleIntervals :: SingleInterval -> SingleInterval -> Maybe (SingleInterval)
_intersectSingleIntervals i1 i2 =
  if i1.end < i2.start || i2.end < i1.start
    then Nothing
    else Just $ mkSingleInterval (max i1.start i2.start) (min i1.end i2.end)

intersect :: Interval -> Interval -> Interval
intersect i1 i2 =
  Interval $
    if null i1.intervals || null i2.intervals
      then []
      else sortOn (.start) $ _merge [] $ sortOn (.start) $ helper i1.intervals i2.intervals
  where
    helper :: [SingleInterval] -> [SingleInterval] -> [SingleInterval]
    helper xs ys = do
      x <- xs
      y <- ys
      case (if x.start <= y.start then _intersectSingleIntervals x y else _intersectSingleIntervals y x) of
        Nothing -> []
        Just i -> [i]

mapIntervals :: (SingleInterval -> SingleInterval) -> Interval -> Interval
mapIntervals f i =
  fromList' $ fmap f $ intervals i

-- | Subtracts two intervals.
_subtractSingleIntervals :: SingleInterval -> SingleInterval -> [SingleInterval]
_subtractSingleIntervals i1 i2 =
  case compare i1.start i2.start of
    LT -> case compare i1.end i2.start of
      -- x  x
      --      y  y
      LT -> [i1]
      -- x  x
      --    y  y
      EQ -> [mkSingleInterval i1.start $ i1.end - 1]
      GT -> case compare i1.end i2.end of
        -- x    x
        --    y  y
        LT -> [mkSingleInterval i1.start $ i2.start - 1]
        -- x     x
        --    y  y
        EQ ->
          [mkSingleInterval i1.start $ i2.start - 1]
        -- x      x
        --    y  y
        GT -> [mkSingleInterval i1.start $ i2.start - 1, mkSingleInterval (i2.end + 1) i1.end]
    EQ -> case compare i1.end i2.end of
      -- x      x
      -- y      y
      EQ -> []
      -- x    x
      -- y     y
      LT -> []
      -- x      x
      -- y     y
      GT -> [mkSingleInterval (i2.end + 1) i1.end]
    GT -> case compare i1.start i2.end of
      --      x   x
      -- y  y
      GT -> [i1]
      --     x   x
      -- y   y
      EQ -> if i1.start == i1.end then [] else [mkSingleInterval (i2.end + 1) i1.end]
      LT -> case compare i1.end i2.end of
        --       x   x
        -- y            y
        LT -> []
        --        x   x
        -- y          y
        EQ -> []
        --         x   x
        -- y         y
        GT -> [mkSingleInterval (i2.end + 1) i1.end]

subtract :: Interval -> Interval -> Interval
subtract i1 i2 =
  Interval $
    sortOn (.start) $
      helper i1.intervals i2.intervals
  where
    helper :: [SingleInterval] -> [SingleInterval] -> [SingleInterval]
    helper xs [] = xs
    helper xs (y : ys) = helper (concatMap (\x -> _subtractSingleIntervals x y) xs) ys
