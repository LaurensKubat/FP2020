module Chain where

import Data.Array

infix 1 :-
data With a b  =  a :- b
  deriving (Show)

instance (Eq a) => Eq (With a b) where
  (a :- _) == (b :- _)  =  a == b
instance (Ord a) => Ord (With a b) where
  (a :- _) <= (b :- _)  =  a <= b

-- Costs and dimensions.

type Cost  =  Integer
type Dim   =  (Integer, Integer)

(***) :: Dim -> Dim -> With Cost Dim
(i, j) *** (j', k)
  | j == j'    =  (i * j * k) :- (i, k)
  | otherwise  =  error "***: dimensions do not match"

liftWithCost :: (a -> a -> With Cost a) -> With Cost a -> With Cost a -> With Cost a
liftWithCost f (c1 :- x1) (c2 :- x2) = c1 + c + c2 :- y where c :- y = f x1 x2

(<***>) :: With Cost Dim -> With Cost Dim -> With Cost Dim
(<***>) = liftWithCost (***)

-- Minimal costs.

minCost :: [Dim] -> With Cost Dim
minCost [a]  =  0 :- a
minCost as   =  minimum [ minCost bs <***> minCost cs | (bs, cs) <- split as ]

split :: [a] -> [([a], [a])]
split []        =  error "split: empty list"
split [_a]      =  []
split (a : as)  =  ([a], as) : [ (a : bs, cs) | (bs, cs) <- split as]

-- minCost [(10, 30), (30, 5), (5, 60)]
-- minCost [ (i, i + 1) | i <- [1 .. 3] ]
-- minCost [ (i, i + 1) | i <- [1 .. 9] ]

minimumCost :: (size -> size -> With Cost size) -> [size] -> With Cost size
minimumCost f xs = [ | (bs, cs) <- split as]


-- concatCost :: [a] -> [a] -> With Cost [a]
-- intAdditionCost :: Integer -> Integer -> With Cost Integer

data Tree elem  =  Leaf elem | Tree elem :^: Tree elem
  deriving (Show, Eq, Ord)

-- optimalChain :: (size -> size -> With Cost size) -> [size] -> With Cost (size, Tree size)

{- Why is (++) declared to be right associative?
-}

-- minimumCostMemo :: (size -> size -> With Cost size) -> [size] -> With Cost size
-- optimalChainMemo :: (size -> size -> With Cost size) -> [size] -> With Cost (size, Tree size)

