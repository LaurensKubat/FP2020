module BinarySearchTree where

-- Laurens Kubat
-- s4626249
import BinaryTree  hiding (member)
import QuickTest

registry :: Tree String
registry = Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty

-- The difference is the complexity of the member function, this function has O(log n) complexity
-- and the other has O(n) complexity
member :: (Ord elem) => elem -> Tree elem -> Bool
member e Empty = False
member e (Node l a r)
    | a == e = True
    | a < e = member e r
    | a > e = member e l

insert :: (Ord elem) => elem -> Tree elem -> Tree elem
insert e Empty = (Node Empty e Empty)
insert e (Node l a r)
    | e >= a = (Node l a (insert e r))
    | e < a = (Node (insert e l) a r)

delete :: (Ord elem) => elem -> Tree elem -> Tree elem
delete e (Node Empty a Empty)
    | e == a = Empty
delete e (Node Empty a (Node rl ra rr ))
    | ra == a = (Node rl ra rr)
    | otherwise = delete e (Node rl ra rr )
delete e (Node l a Empty)
    | a == e = l
    | otherwise = delete e l
delete e (Node (Node ll la lr) a (Node rl ra rr))
    | e == a = (Node (Node ll la lr) ra rr ) 
    | a < e = delete e (Node rl ra rr)
    | a > e = delete e (Node ll la lr)   

-- isSearchTree :: (Ord elem) => Tree elem -> Bool
-- trees :: [elem] -> Probes (Tree elem)  -- should be defined in BinaryTree
