module DefaultMap where

import Common ((|>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

data DefaultMap k v = DefaultMap
  { defaultValue :: v,
    values :: Map.Map k v
  }

empty :: Ord k => v -> DefaultMap k v
empty defaultValue = DefaultMap {values = mempty, ..}

set :: Ord k => k -> v -> DefaultMap k v -> DefaultMap k v
set k v dm = dm {values = Map.insert k v dm.values}

update :: Ord k => k -> (v -> v) -> DefaultMap k v -> DefaultMap k v
update k vf dm = set k (vf $ get k dm) dm

get :: Ord k => k -> DefaultMap k v -> v
get k dm = fromMaybe dm.defaultValue $ Map.lookup k dm.values

fromList :: Ord k => [(k, v)] -> v -> DefaultMap k v
fromList l defaultV = empty defaultV |> flip (foldr (uncurry set)) l
