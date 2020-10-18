data BTree a = Tip a | Bin (BTree a) (BTree a)

mapBTree :: (a -> b) -> BTree a -> BTree b
mapBTree f (Tip a)     = Tip (f a)                             -- (1)
mapBTree f (Bin t1 t2) = Bin (mapBTree f t1) (mapBTree f t2)   -- (2)

foldBTree :: (a -> a -> a) -> BTree a -> a
foldBTree f (Tip x)     = x                                    -- (3)
foldBTree f (Bin t1 t2) = f (foldBTree f t1) (foldBTree f t2)  -- (4)

tips :: (BTree a) -> [a]
tips t = foldBTree (++) (mapBTree (:[]) t)                     -- (5)

{-
Explain what foldBTree and tips do
foldBTree is a higher order function that applies f to each tip of the B tree.
Tip is a leaf in the binary tree, only the tips contain data
-}

{-
To prove: map f (tips t) = tips (mapBTree f t) for all f,t
base case t = Tip a
map f (tips t)
 = (5)
map f (foldBtree (++) mapBtree (:[]) t)
 = (1)
map f (foldBtree (++) [a])
 = (3)
map f ([a])
 = (definition of map)
f a

and

tips (mapBTree f t)
 = (1)
tips (Tip (f a))
 = (5)
FoldBTree (++) (MapBTree (:[]) (f a))
 = (1)
FoldBTree (++) [f a]
 = (3)
f a
thus the base case is proven

Induction step with induction hypethesis
map f (tips t) = tips (mapBTree f t) for all f,t

map f (tips Bin l r) = tips (mapBTree f Bin l r)
    = (5)
map f (foldBTree (++) (mapBTree (:[]) l r)) = tips (mapBTree f Bin l r)
    = (2)
map f (foldBTree (++) Bin (mapBTree (:[]) l) (mapBTree (:[]) r)) 
        = tips (mapBTree f Bin l r)
    = (3)
map f (foldBTree (++) Bin (mapBTree (:[]) l) (mapBTree (:[]) r)) 
        = tips (Bin (mapBTree f l) (mapBTree f r))
    = (5)
map f (foldBTree (++) Bin (mapBTree (:[]) l) (mapBTree (:[]) r)) 
        = foldBTree (++) mapBTree (:[] Bin (mapBTree f l) (mapBTree f r))
    = (4)
map f (foldBTree (++) mapBTree (:[]) l) ++ foldBTree (++) mapBTree (:[]) r))
        = foldBTree (++) mapBTree (:[] Bin (mapBTree f l) (mapBTree f r))
    = (2)
map f (foldBTree (++) mapBTree (:[]) l) ++ foldBTree (++) mapBTree (:[]) r))
        = foldBTree (++) Bin mapBTree (:[]) mapBTree f l mapBTree (:[]) mapBTree f r
    = (4)
map f (foldBTree (++) mapBTree (:[]) l) ++ foldBTree (++) mapBTree (:[]) r))
        = foldBTree (++) mapBTree (:[]) mapBTree f l ++ foldBTree (++) mapBTree (:[]) mapBTree f r
    = (5) (in reverse)
map f (tips l ++ tips r) = tips (mapBTree f l) ++ tips (mapBTree f r)
    = (Proof from ListInduction)
map f (tips l) ++ map f (tips r) = tips (mapBTree f l) ++ tips (mapBTree f r)
    = IH on l
map f (tips r) = tips (mapBTree f r)
and after applying IH on r, we see this clearly holds
-}
