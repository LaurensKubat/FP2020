module QuickTest (Probes, Property, (?->), (?=>)) where

-- Laurens Kubat
-- s4626249

import Prelude hiding ((<*>))
import Data.List (sort) 

type Probes a    =  [a]

type Property a  =  a -> Bool

infixr 1  ?->, ?=>

(?->)   :: Probes a -> Property b -> Property (a -> b)
(?=>)   :: Probes a -> (a -> Property b) -> Property (a -> b)

probes ?-> prop  =  \f -> and [ prop (f x)   | x <- probes ]
probes ?=> prop  =  \f -> and [ prop x (f x) | x <- probes ]

ordered :: (Ord a) => Property [a]
ordered (x:[]) = True
ordered (x:y:xs) = x >= y && ordered (y:xs)

permutations :: [a] -> Probes [a]
-- This basically returns [[a]] since Probes a = [a], thus we can use the Data.list implementation of permutations!
-- permutations xs = List.permutations xs
-- since that would be cheating though, below is an actual implementation, I have my own implementation, only that still has duplicates in it
-- (and isnt very efficient)
permutations xs = permutate (length xs) xs 
 
-- permutate adds an element to the list and permutates it and returns all possible lists
permutate ::  Int -> [a] -> [[a]]
permutate _ (x:[]) = [[x]]
permutate 1 (x:xs) = permute x xs
permutate n (x:xs) = permute x xs ++ permutate (n-1) (xs ++ [x])

permute :: a -> [a] -> [[a]]
permute x xs = perm (length xs) x xs 

perm :: Int -> a -> [a] -> [[a]]
perm 0 x xs = [insertAt 0 x xs]
perm n x xs = insertAt n x xs : perm (n-1) x xs

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x xs = (x:xs)
insertAt n x (y:xs) = y : insertAt (n-1) x xs

isRuns :: Ord a => Property ([a] -> [[a]])
isRuns  = probesRuns ?-> isRun

probesRuns :: Ord a => [a]
probesRuns = []

-- We define is Run to check whether a list is a run
isRun :: Ord a => [a] -> Bool
isRun (x:[]) = True
isRun (x:y:xs) = x <= y && isRun (y:xs)

-- Runs from Runs.hs so i dont have to import it
runs :: (Ord a) => [a] -> [[a]]
runs [] = [[]]
runs [_] = [[]]
runs xs = runsHelp $ subLists xs

runsHelp :: (Ord a) => [[a]] -> [[a]]
runsHelp [[]] = [[]]
runsHelp [] = []
runsHelp (x:[]:[]) = [x]
runsHelp (x:y:xs)
  | last x <= head y = runsHelp $ (concat [x, y]) : xs
  | otherwise = x : runsHelp (y:xs)

subLists :: [a] -> [[a]]
subLists [] = [[]]
subLists (x:xs) = [x] : subLists xs

isqrt :: Integer -> Integer
isqrt n = loop 0 3 1
  where loop i k s  | s <= n      = loop (i + 1) (k + 2) (s + k)
                    | otherwise  = i

isIntegerSqrt :: Property (Integer -> Integer)
isIntegerSqrt = map isqrt intProbes ?=> \x y -> y*y == x

intProbes :: [Integer]
intProbes = [1,2,3,4,5,6,7,8,9]

-- we have as * bs items that we need to produce
infixr 4  <*>
(<*>) :: Probes a -> Probes b -> Probes (a, b)
(<*>) as bs = [(x,y) | x <- as, y <- bs]
 
niftySort :: [a] -> [a]
niftySort _xs  =  []

trustedSort :: (Ord a) => [a] -> [a]
trustedSort  =  sort
