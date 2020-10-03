module BinaryTree where

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving (Show)

instance Functor Tree where
  fmap _f Empty         =  Empty
  fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

-- exercise 1
ex :: Tree Char
ex = Node a 'c'  f
a :: Tree Char
a = Node Empty 'a' (Node Empty 'b' Empty)
f :: Tree Char
f = Node (Node Empty 'd' Empty) 'f' (Node Empty 'g' Empty)
-- exercise 2

ex1 :: Tree Integer
ex1 = Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
{-
        4711
      /     \
    Empty    0815
            /     \
          Empty    42
                  /   \
                Empty Empty

-}
ex2 :: Tree String
ex2 = Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
{-
          Ralf
          /   \
      Peter  Empty
      /    \
    Frits   Empty
    /    \
  Empty   Empty
-}
ex3 :: Tree Char
ex3 = Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)
{-
      k
    /   \
   a     z
  /\     /\ 
 Empty  Empty

-}

size :: Tree elem -> Int
size Empty = 0 
size (Node l a r) = 1 + size l + size r

-- We look for the min depth in the side branches of the root
minHeight :: Tree elem -> Int
minHeight Empty = 0
minHeight (Node l a r)
  | lH <= rH = lH + 1
  | rH < lH = rH + 1
    where 
      lH = minHeight l
      rH = minHeight r

-- We look for the max depth in the side branches of the root.
maxHeight :: Tree elem -> Int
maxHeight Empty = 0
maxHeight (Node l a r)
  | lH >= rH = lH + 1
  | rH > lH = rH + 1
    where 
      lH = maxHeight l
      rH = maxHeight r


member :: (Eq elem) => elem -> Tree elem -> Bool
member e Empty = False
member e (Node l a r)
  | e == a = True
  | otherwise = (member e l) || (member e r) 

-- Preorder iterates the entire tree (each element) once recursively, thus O(n)
preorder :: Tree elem -> [elem]
preorder Empty = []
preorder (Node l a r) = a : preorder l ++ preorder r 

-- inorder iterates the entire tree (each element once) recursively and does the comparisons
-- to order the list during this iteration. Thus O(n)
inorder :: (Ord elem) => Tree elem -> [elem]
inorder Empty = []
inorder (Node Empty a Empty) = [a]
inorder (Node Empty a r)
  | (head inR) > a = a : inR
  | otherwise = inR ++ [a]
        where inR = inorder r
inorder (Node l a Empty)
  | (head $ inL) > a = a : inL
  | otherwise = inL ++ [a]
    where inL = inorder l
inorder (Node l a r)
  | (head inL) > a && (head inR) > a && (head $ inL) > (head inR) = [a] ++ inR ++ inL
  | (head inL) > a && (head inR) > a && (head $ inL) < (head inR) = [a] ++ inL ++ inL
  | (head inL) < a && (head inR) > a && (head $ inL) < (head inR) = inL ++ [a] ++ inR
  | (head inL) < a && (head inR) < a && (head $ inL) < (head inR) = inL ++ inR ++ [a]
  | (head inL) < a && (head inR) < a && (head $ inL) > (head inR) = inR ++ inL ++ [a]
  | (head inL) > a && (head inR) < a && (head $ inL) > (head inR) = inR ++ [a] ++ inL
    where 
      inL = inorder l
      inR = inorder r

-- Postorder iterates the entire tree recursively once, thus O(n)
postorder :: Tree elem -> [elem]
postorder Empty = []
postorder (Node l a r) = postorder l ++ postorder r ++ [a]

layout :: (Show elem) => Tree elem -> String
layout Empty = ""
layout t = layoutH 1 t

layoutH :: (Show elem) => Int -> Tree elem -> String
layoutH _ Empty = ""
layoutH cur (Node l a r)= layoutH (cur+1) l ++ (replicate (cur * 3) ' ') ++"/\n"++ (replicate (cur * 3) ' ') ++"-" ++ show a ++ "\n" ++ (replicate (cur * 3) ' ') ++ "\\\n" ++ layoutH (cur+1) r

buildTest :: [Integer]
buildTest = [1,2,3,4,5,6,7,8]

-- given that the shape does not matter, the following solution works, but it is basically a list
build :: [elem] -> Tree elem
build [] = Empty
build (x:xs) = Node (build xs) x Empty

balanced :: [elem] -> Tree elem
balanced [] = Empty
balanced (x:xs) = Node ( balanced first) x (balanced second)
  where
    first = f
    second  = s
    (f, s) = splitAt ((length xs) `div` 2) xs


-- create :: Int -> Tree ()
-- Since every creation of an element is of O(1) and we have n elements. I think Harry Hacker is a quack, since there is no way to
-- do less creations of nodes in the tree than n. Thus it is not possible to have a better complexity than O(n)
