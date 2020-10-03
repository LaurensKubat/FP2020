module ListFunctions where

-- Laurens Kubat
-- s4626249

import Prelude hiding (member)

allTrue :: [Bool] -> Bool
-- allTrue xs = foldr (\x acc -> x && acc) True xs
-- or using foldr1, which makes more sense here since the first item of the list can be used as the starting value
allTrue xs = foldr1 (\x acc -> x && acc) xs

allFalse :: [Bool] -> Bool
-- allFalse xs = foldl (\acc x -> acc && x) False xs
-- This one also makes more sense to do using foldl1 (or foldr1)
allFalse xs = foldl1 (\acc x -> acc && x) xs


member :: (Eq a) => a -> [a] -> Bool
member e xs = foldr (\x acc -> if x == e then True else False) False xs

smallest :: [Int] -> Int
-- since we want to find the smallest the most logical starting value is the first item of the list
smallest xs = foldl1 (\acc x -> if x < acc then x else acc) xs

largest :: [Int] -> Int
-- here we also 
largest xs = foldr1 (\x acc -> if x > acc then x else acc) xs

