module Generate where

import Control.Monad ( replicateM )

bools :: [Bool]
bools = pure False ++ pure True

maybes :: [elem] -> [Maybe elem]
maybes elems = pure Nothing ++ (pure Just <*> elems)

data Suit  =  Spades | Hearts | Diamonds | Clubs
data Rank  =  Faceless Integer | Jack | Queen | King
data Card  =  Card Rank Suit | Joker

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

cards :: [Card]
cards = Joker : [Card x y | x <- ranks, y <- suits]

suits :: [Suit]
suits = [Spades, Hearts, Diamonds, Clubs]

ranks :: [Rank]
ranks = faceless [2..10] ++ [Jack, Queen, King]

faceless :: [Integer] -> [Rank]
faceless (x:xs) = Faceless x : faceless xs 

-- using replicateM, we can easily implement a lists generator
lists :: [elem] -> Int -> [[elem]]
lists es n = replicateM n es

-- to then create all possible tries is pretty easy, we simply apply
trees :: [elem] -> Int -> [Tree elem]
trees es n = map treeify $ lists es n

-- treeify is the balanced tree builder from assignment 4
treeify :: [elem] -> Tree elem
treeify [] = Empty
treeify (x:xs) = Node ( treeify first) x (treeify second)
  where
    first = f
    second  = s
    (f, s) = splitAt ((length xs) `div` 2) xs
    
l1 = lists bools 1
l2 = lists bools 2
tr1 = trees (lists bools 2) 1
t2 = trees (lists bools 2) 2
