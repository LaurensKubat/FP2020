module Sublists where

-- Laurens Kubat
-- s4626249

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = [x:sublist | sublist <- subs xs] ++ subs xs


{-
If xs has n elements, how many elements does the list (subs xs) have?
counting [], we have 2^n subsequences
-}
