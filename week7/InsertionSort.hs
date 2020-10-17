module InsertionSort where

insert :: (Ord a) => a -> [a] -> [a]
insert a []    =  [a]                                   -- (1)
insert a (b : xs)
  | a <= b     =  a : b : xs                            -- (2)
  | otherwise  =  b : insert a xs                       -- (3)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort []        =  []                           -- (4)
insertionSort (x : xs)  =  insert x (insertionSort xs)  -- (5)

ordered :: Ord a => [a] -> Bool
ordered []  = True                                      -- (6)
ordered [x] = True                                      -- (7)
ordered (x:y:xs) = x <= y && ordered (y:xs)             -- (8)

{-
To prove: ordered xs => ordered (insert x xs)
base case: case xs = [] and arbitrary x
  ordered [] = True (6)

  ordered (insert x [])
    = (1)
  ordered ([x])
    = (7)
  True

  Inductive step
  case xs (inductive step)
  with Induction Hypothesis: ordered(xs) => ordered(insert x xs)
  ordered(xs) = True

  ordered(insert x xs)
   = 
  ordered(insert x (b:xs))
  if a > b then 
    (
    if a > b, there is a recursive call, i am not sure if i should prove
    anything at this point, but it seemed reasonable to me to at least
    show that a recursive call happens
    )
    b :insert a xs (3)
  if a <= b then
    a : b : xs (2)
    = IH
  ordered(xs)
 
-}

{-
To prove: ordered (insertionSort xs)
base case xs = []
ordered (insertionSort [])
 = (4)
ordered ( [] )
 = (6)
True

Inductive step 
with IH: ordered (InsertionSort(x:xs)) = ordered (insert x InsertionSort (xs))
ordered (InsertionSort(x:xs))
 = (5)
ordered (insert x InsertionSort (xs))
(in the proof above, we showed that insert x xs holds if xs is sorted, thus)
 = IH
ordered (insertionSort(xs))
-}

