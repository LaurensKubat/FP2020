module Unfold where

import Data.List
-- Laurens Kubat
-- s4626249

import Prelude hiding (take,zip,(++))

take :: Int -> [a] -> [a]
take n xs = unfoldr (\n -> if n == 0 then Nothing else Just x) n
-- zip :: [a] -> [b] -> [(a,b)]
-- fibs :: [Integer]
-- primes :: [Integer]

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo rep seed = produce seed
  where
  produce seed = case rep seed of
    Left l      -> l
    Right(a,ns) -> a : produce ns

-- unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
-- (++) :: [a] -> [a] -> [a]
-- insert :: (Ord a) => a -> [a] -> [a]

