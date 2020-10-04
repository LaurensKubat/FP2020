module Numeral where

-- Laurens Kubat
-- s4626249

type Base   =  Integer
type Digit  =  Integer

msdf :: Base -> [Digit] -> Integer
-- we start the most significant numbers, and multiply the accumalated total acc with the base for each element in the list
msdf b xs = foldl (\acc x -> acc * b + x) 0 xs

lsdf :: Base -> [Digit] -> Integer
lsdf b xs = foldr (\x acc -> acc * b + x) 0 xs

test :: [Integer]
test = [1,0,1,1]
-- msdf 2 test should be 11, lsdf 2 test should be 13