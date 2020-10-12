module DigitalSorting where

-- Laurens Kubat
-- s4626249

import Data.List (groupBy, sortBy, nub)

class Rank key where
  rank :: [(key, val)] -> [[val]]
  
sortKey :: Rank key => [(key, val)] -> [val]
sortKey = concat . rank

genericSortKey :: (Ord key) => [(key, val)] -> [val]
genericSortKey kvs  = map snd (sortBy (\kv1 kv2 -> compare (fst kv1) (fst kv2)) kvs)

instance Rank () where
  rank []  = []
  rank kvs = [map snd kvs]

-- Since sorting based on keys returns non empty runs of values where each
-- run has an equal key, from this, i gather that sorting the keys would
-- simply be removing equal keys, since we do not have anything to order the
-- keys on. If Rank key had an instance of eq, it would look like the following 
-- sort' :: Rank key => [key] -> [key]
-- sort' xs = nub xs 
-- but since this is not the case. I am not sure what to do

{-
For each item, if the key is False, return in the first List,
   else return in the second List
-}

instance Rank Bool where 
  rank [] = []
  rank ((b, x):xs)
    | b = [x : trueBucket, falseBucket]
    | not b = [trueBucket, x : falseBucket]
      where 
        trueBucket = head $ rank xs
        falseBucket = last $ rank xs


genericRank :: (Ord key) => [(key, val)] -> [[val]]
genericRank kvs = map (map snd) (groupBy (\(k1, v1) (k2, v2) -> k1 == k2) kvs)

-- just as that I didnt figure out how to do sort, i can manage compare here
-- because I dont understand how we're supposed to compare keys if there
-- are no Eq or Ord constraints
-- compare' :: Rank key => key -> key -> Ordering

{-
for instance (Rank key1, Rank key2) => Rank (key1, key2) where
whats supposed to happen, is that if key1 and key2 are equal, the
values are placed under the same (key1, key2) pair,
but again, I cant manage to do the comparison part
-}
-- instance (Rank key1, Rank key2) => Rank (key1, key2) where
--   rank :: (Rank key1, Rank key2) => [(key1, key2), val] -> [[val]]

-- instance (Rank key1, Rank key2) => Rank (Either key1 key2) where

type ListRepr elem  =  Either () (elem, [elem])

toListRepr :: [elem] -> ListRepr elem
toListRepr []        =  Left ()
toListRepr (a : as)  =  Right (a, as)

-- instance (Rank key) => Rank [key] where

