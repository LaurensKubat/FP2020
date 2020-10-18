module TreeInduction where

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving (Show)

{-
1: Give an induction scheme for trees
Each Tree can either be Empty, or have two Tree's and an element
-}

{-
2: Implement inner, outer
-}

inner :: Tree a -> Int
inner Empty = 0                             -- (1)
inner (Node l _ r) = inner l + inner r + 1  -- (2)

outer :: Tree a -> Int
outer Empty = 1                             -- (3)
outer (Node l _ r) = outer l + outer r      -- (4)

{-
2: To prove: outer t = inner t + 1
base case t = Empty
inner Empty = 0
outer Empty = 1
Thus outer t = inner t + 1 holds for the base case
Inductive step: outer t = inner t + 1
For t = Node l e r
outer l e r
  = (4)
outer l + outer r
  = IH
(inner l + 1 + inner r + 1)
  =
(inner l + inner r + 1) + 1
  = (2)
inner (Node l _ r) + 1
QED
-}

size :: Tree a -> Int
size Empty = 0                          --(1)
size (Node l _ r) = size l + size r + 1 --(2)

minHeight :: Tree a -> Int
minHeight Empty = 0                     --(3)
minHeight (Node l _ r) = min (minHeight l) (minHeight r) + 1 --(4)

maxHeight :: Tree a -> Int
maxHeight Empty = 0                     --(5)
maxHeight (Node l _ r) = max (maxHeight l) (maxHeight r) + 1 --(6)

{-
3: To prove: 2^minHeight t - 1 <= size t <= 2^maxHeight t - 1
(2 * (min ab) <= a + b) Hint from exercise
base case:
t = Empty
2 ^ (minHeight Empty - 1) <= size Empty <= 2^maxHeight Empty - 1
  = (3)
2^0 -1 <= size Empty <= 2^maxHeight Empty -1
  = (1)
0 <= 0 <= 2^maxHeight Empty - 1
  = (5)
0 <= 0 <= 0

Induction Hypethesis: 2^minHeight t - 1 <= size t <= 2^maxHeight t - 1


2^minheight (Node l e r) -1 <= size (Node l e r) <= 2^maxHeight (Node l e r) -1
  =(2, 4 and 6)
2^(min(minHeight l minHeight r) + 1) -1 <= size l + size r + 1 
    <= 2^(max (maxHeight l maxheight r) + 1) -1
  = assuming minHeight l < minHeight r
2^(minHeight l + 1) - 1 <= size l + size r + 1 <= 2^(max (maxHeight l maxheight r) + 1) -1
  = assuming maxHeight r > maxHeight l
2^(minHeight l + 1) - 1 <= size l + size r + 1 <= 2^(maxheight r) + 1) -1
  =
2^(minHeight l) - 1 <= size l + size r <= 2^(maxheight r) - 1
  = IH on minHeight l and size l proves the minHeight part,
    IH on size r and maxHeight size r proves the maxHeight part
This proof would look pretty much the same if minHeight l > minHeight r only
with l an r interchanged. Same goes for maxHeight l  and r
-}
