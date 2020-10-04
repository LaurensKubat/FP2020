module Unfold where

import Data.List ( unfoldr )
import Data.Either
-- Laurens Kubat
-- s4626249

import Prelude hiding (take,zip,(++))

take :: Int -> [a] -> [a]
take n xs = unfoldr (
  \(xs, n) -> case xs of
    [] -> Nothing
    x:xs -> if n == 0 then Nothing else Just (x, (xs, n-1))
    ) (xs, n)


takeTest :: [Char]
takeTest = ['a','b','c','d']

-- for zip, we use a two nested 'case of' expressions to check if either list is empty, if on of the tw is empty, we return nothing
-- else we return tuple
zip :: [a] -> [b] -> [(a,b)]
zip xs ys = unfoldr (\(xs ,ys) -> case xs of
  [] -> Nothing
  x:xs -> case ys of
    [] -> Nothing
    y:ys -> Just ((x, y), (xs, ys))
    ) (xs, ys)

zTestA :: [Integer]
zTestA = [1,2,3,4]
zTestB :: [Integer]
zTestB = [1,2]

fibs :: [Integer]
fibs = unfoldr (\(x, y) -> Just (x+y, (y, x+y))) (0, 1)

f :: [Integer]
f = take 5 fibs

-- we base the implementation on the following reference from the assignment
-- primes = sieve [2..] where
-- sieve (p:xs) = p : sieve [ n | n ← xs, n ‘mod‘ p /= 0 ]

primes :: [Integer]
primes = unfoldr (\xs -> case xs of 
  [] -> Nothing
  (x:xs) -> Just (x, ([ n | n <- xs, n `mod` x /= 0]))) [2..]

p :: [Integer]
p = take 5 primes

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo rep seed = produce seed
  where
  produce seed = case rep seed of
    Left l      -> l
    Right(a,ns) -> a : produce ns

-- what we need to do is wrap the function returning a Maybe value into a function returning an Either value
unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
unfoldrApo f t = apo (\t -> case x of
  Nothing -> Left []
  Just (ax, tx) -> Right (ax, tx)) t
  where x = f t


(++) :: [a] -> [a] -> [a]
(++) xs ys = unfoldrApo (\(xs, ys) -> case xs of
  x:xs -> Just (x, (tail xs, ys))
  [] -> case ys of
    y:ys -> Just (y, (xs, tail ys))
    [] -> Nothing) (xs, ys)


insert :: (Ord a) => a -> [a] -> [a]
insert x ys = unfoldrApo ((x, ys) -> case ys of
  [] -> Nothing
  y:z:ys -> if y < x < z then Just (x, (x, z:xs)) else Just (y, (x, z:xs))
  ) (x, ys)

