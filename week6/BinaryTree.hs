{-# LANGUAGE InstanceSigs #-}
module BinaryTree where

-- Laurens Kubat
-- s4626249

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving Show

ex1 :: Tree Integer
ex1 = Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
ex2 :: Tree String
ex2 = Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
ex3 :: Tree Char
ex3 = Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)

instance (Eq elem) => Eq (Tree elem) where
  (==), (/=) :: Tree elem -> Tree elem -> Bool
  (==) Empty l = False
  (==) l Empty = False
  (==) (Node la ea ra) (Node lb eb rb) = ea == eb || la == lb || ra == rb
  (/=) a b = not $ a == b

-- The implementation of <= depends on the expected use, this can be a size
-- comparison, or a comparison of the sum of all elements or just the 
-- top two elements
instance (Ord elem) => Ord (Tree elem) where
  (<=) :: Tree elem -> Tree elem -> Bool
  (<=) (Node _ ea _) (Node _ eb _) = ea <= eb


instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Node l e r) = (Node (fmap f l) (f e) (fmap f r))

