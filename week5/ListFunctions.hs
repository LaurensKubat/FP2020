module ListFunctions where

-- Laurens Kubat
-- s4626249

import Prelude hiding (member)

allTrue :: [Bool] -> Bool
-- allTrue xs = foldr (\x acc -> x && acc) True xs
-- or using foldr1, which makes more sense here since the first item of the list can be used as the starting value
allTrue xs = foldr1 (\x acc -> x && acc) xs

allFalse :: [Bool] -> Bool
-- allFalse xs = foldl (\acc x -> acc && x) True xs
-- This one also makes more sense to do using foldl1 (or foldr1), the not is needed since we want to return true if all are false
allFalse xs = not $ foldl1 (\acc x -> acc || x) xs


member :: (Eq a) => a -> [a] -> Bool
member e xs = foldr (\x acc -> if x == e then True else False) False xs

smallest :: [Int] -> Int
-- since we want to find the smallest the most logical starting value is the first item of the list
smallest xs = foldl1 (\acc x -> if x < acc then x else acc) xs
-- only using foldl would look like this, where we use the result of maxBound Int as the starting value
-- smallest xs = foldl (\acc x -> if x < acc then x else acc) 9223372036854775807 xs

largest :: [Int] -> Int
-- here we also use foldr1
largest xs = foldr1 (\x acc -> if x > acc then x else acc) xs
-- using foldr, we would do something similar to smallest, using the result minBound Int
-- largest xs = foldr (\x acc -> if x > acc then x else acc) (-9223372036854775807) xs

