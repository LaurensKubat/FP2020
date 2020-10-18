
{-
To prove: map f (as ++ bs) = (map f as) ++ (map f bs)
base case as = [] and bs = []
map f ([] ++ []) = (map f []) ++ (map f [])
    = (1)
map f ([]) = (map f []) ++ (map f [])
    = (3)
[] = [] ++ []
    = (1)
[] = []

Induction step
induction hypothesis: map f (as ++ bs) = (map f as) ++ (map f bs)
we prove with induction on as
map f ((a:as) ++ (bs)) = (map f a:as) ++ (map f bs)
    = (2)
map f (a : (as ++ bs)) = (map f a:as) ++ (map f bs)
    = (4)
map f (a : (as ++ bs)) = (f a : map f as) ++ (map f bs)
    = (4)
f a : map f (as ++ bs) = f a : (map f as) ++ (map f bs)
    = IH
f a = f a

Induction on bs would look pretty much the same

-}

{-
To prove: concat (map (map f) xs) = map f (concat xs)
base case  xs = []
concat (map (map f) []) = map f (concat [])
    = (3) applying map with function (map f) to []
concat ([]) = map f (concat [])
    = (5) on the right side of the equation
concat ([]) = map f ([])
    = (5) on the left side of the equation
[] = map f ([])
    = (3)
[] = []
This this holds for the base case

IH: concat (map (map f) xs) = map f (concat xs)
we prove:
concat (map (map f) x:xs) = map f (concat x:xs)
    = (4)
concat (map f x map (map f) xs) = map f (concat x:xs)
    = (6)
concat (map f x map (map f) xs) = map f (x ++ concat xs)
    = (6)
map f x ++ concat map (map f) xs = map f (x ++ concat xs)
    = (4)
map f x ++ concat map (map f) xs = map f x ++ map f concat xs
    = IH 
map f x = map f x
Thus this holds

-}
