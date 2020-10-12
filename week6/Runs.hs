module Runs where

-- Laurens Kubat
-- s4626249

import Data.List

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
