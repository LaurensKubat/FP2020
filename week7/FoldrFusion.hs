module FoldrFusion where

import Prelude hiding (map)

{-
The fusion law for foldr states that if
  f (g x y) = h x (f y)   for all x,y
then
  f . foldr g e = foldr h (f e)
-}

map :: (a -> b) -> [a] -> [b]
map f xs = foldr (\x acc -> (f x):acc) [] xs

{-
To prove:  
foldr g e . map f = foldr (g . f) e
  = (definiton of map)
foldr g e . foldr f' = foldr (g . f) e (where f' = (\x acc -> (f x):acc))
  =  renaming foldr f' to h 
h . foldr g e = foldr (g . f) e
  = definition of (.)
h . foldr g e = foldr g (f e)
Which is equal according to the foldr fusion law
-}

{-
To prove:  map (f . g) = map f . map g
  map (f . g) = map f . map g
    = (definition of map)
  foldr h = map f . foldr i (where h = (\x acc -> f . g x:acc) and i = (\x acc -> g x:acc)
  which is equal according to the foldr-map law
  -}

reduce :: Monoid a => [a] -> a
reduce = foldr (<>) mempty

{-
To prove:  
reduce . concat = reduce . map reduce
  = definition of reduce
foldr (<>) mempty . concat = foldr (<>) mempty . map foldr (<>) mempty
  =


-}


