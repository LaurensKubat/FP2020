{-# LANGUAGE FlexibleContexts #-}
module Huffman where

--------------------------------------------------------------------------------

-- Warm-up: constructing a frequency table.

infix 1 :-
data With a b  =  a :- b
  deriving (Show)

instance (Eq a) => Eq (With a b) where
  (a :- _) == (b :- _)  =  a == b
instance (Ord a) => Ord (With a b) where
  (a :- _) <= (b :- _)  =  a <= b

sndelem :: (Ord a) => a -> [With Int a] -> Bool
sndelem _ [] = False
sndelem x ((_ :- z):ys) = x == z || sndelem x ys

updateEntry :: (Ord a) => a -> [With Int a] -> [With Int a]
updateEntry x ((y :- z):ys)
  | x == z = ((y + 1) :- z) : ys
  | otherwise = (y :- z) : updateEntry x ys

frequencies  ::  (Ord a) => [a] -> [With Int a]
frequencies (x:[]) = [1 :- x]
frequencies (x:xs)
  | sndelem x table = updateEntry x table
  | otherwise = [1 :- x] ++ table
    where table = frequencies xs

--------------------------------------------------------------------------------

-- Constructing a Huffman tree.

data Tree elem  =  Leaf elem | Tree elem :^: Tree elem
  deriving (Show, Eq, Ord)

-- easy quicksort implementation to sort the frequencies
huffmanSort :: [With Int a] -> [With Int a]
huffmanSort [] = []
huffmanSort (x:xs) = smaller ++ [x] ++ bigger
  where 
    smaller = huffmanSort [a | a <- xs, a <= x]
    bigger = huffmanSort [a | a <- xs, a > x]

-- leafify transforms [Int :- a] into a list of leafs of the same pair
leafify :: [With Int a] -> [With Int (Tree a)]
leafify [] = []
leafify ((x :- y):xs) =  (x :- Leaf y) : leafify xs

-- insertOrd inserts an item into an ordered list in the correct position
insertOrd :: (Ord a) =>  With a b -> [With a b]-> [With a b]
insertOrd x [] = [x]
insertOrd x (y:ys)
  | x <= y = (x:y:ys)
  | otherwise = y : insertOrd x ys

combine :: (Num a) => With a (Tree b) -> With a (Tree b) -> With a (Tree b)
combine (x :- y) (a :- b) = (x + a :-  y :^: b)

-- combineList combines a list using combine and insertOrd
combineList :: (Ord a, Num a) => [With a (Tree b)] -> [With a (Tree b)]
combineList (x:y:[]) = [combine x y]
combineList (x:y:xs) = combineList $ insertOrd (combine x y) xs

huffman :: [With Int a] -> Tree a
huffman xs =  (\((_ :- y):_) -> y) $ combineList $ leafify $ huffmanSort xs

-- relativeFrequencies is the list with all frequencies 
-- from https://en.wikipedia.org/wiki/Letter_frequency where each
-- frequency is multiplied by 1000 to make the frequencies integers
relativeFrequencies :: [With Int Char]
relativeFrequencies = [8200 :- 'a', 1500 :- 'b', 2800 :- 'c', 4300 :- 'd', 13000 :- 'e', 2200 :- 'f', 2000 :- 'g', 6100 :- 'h', 7000 :- 'i', 150 :- 'j', 770 :- 'k', 4000 :- 'l', 2400 :- 'm', 6700 :- 'n', 7500 :- 'o', 1900 :- 'p', 95 :- 'q', 6000 :- 'r', 6300 :- 's', 9100 :- 't', 2800 :- 'u', 980 :- 'v', 2400 :- 'w', 150 :- 'x', 2000 :- 'y', 74 :- 'z']

huffFreq :: Tree Char
huffFreq = huffman relativeFrequencies
--------------------------------------------------------------------------------

-- Encoding ASCII text.

data Bit = O | I
  deriving (Show, Eq, Ord)

encode :: (Eq a) => Tree a -> [a] -> [Bit]
encode x (y:ys) = getBit y encoding ++ encode x ys
  where encoding = codes x
encode _ [] = []

getBit:: (Eq a) => a -> [(a, [Bit])] -> [Bit]
getBit x ((y, z):ys)
  | x == y = z
  | otherwise = getBit x ys

codes :: Tree a -> [(a, [Bit])]
codes (a :^: b) = (prependLeft $ codes a) ++ (prependRight $ codes b)
codes (Leaf a) = [(a, [])]

-- prependLeft places an O for each the bit sequences of each element 
prependLeft :: [(a, [Bit])] -> [(a, [Bit])]
prependLeft ((y, x):xs) = (y, O : x) : prependLeft xs
prependLeft [] = []

-- prependRight places an I for each bit sequences of each element
prependRight :: [(a, [Bit])] -> [(a, [Bit])]
prependRight ((y, x):xs) = (y, I : x) : prependLeft xs
prependRight [] = []

--------------------------------------------------------------------------------

-- Decoding a Huffman binary.

-- We want to iterate over the tree, going left for each O and right for each I 
-- until we find a leaf with a value.
decode :: Tree a -> [Bit] -> [a]
decode x ys = a : decode x zs
  where (a, zs) = decChar x ys
decode _ [] = []

-- decChar decodes a single character according to the huffman tree
decChar :: Tree a -> [Bit] -> (a, [Bit])
decChar (l :^: r) (y:ys)
  | y == O = decChar l ys
  | y == I = decChar r ys
decChar (Leaf x) ys = (x, ys) 
decChar (Leaf x) [] = (x, [])


--------------------------------------------------------------------------------

-- Some test data.

hw, why :: String
hw = "hello world"

code = huffman (frequencies hw)
enc = encode code hw
dec = decode code enc
correct = dec == hw

why =
  "As software becomes more and more complex, it\n\
  \is  more  and  more important to structure it\n\
  \well.  Well-structured  software  is  easy to\n\
  \write,   easy   to   debug,  and  provides  a\n\
  \collection  of modules that can be re-used to\n\
  \reduce future programming costs. Conventional\n\
  \languages place a conceptual limit on the way\n\
  \problems   can   be  modularised.  Functional\n\ 
  \languages  push  those  limits  back. In this\n\
  \paper we show that two features of functional\n\
  \languages    in    particular,   higher-order\n\
  \functions and lazy evaluation, can contribute\n\
  \greatly  to  modularity.  Since modularity is\n\
  \the key to successful programming, functional\n\
  \languages  are  vitally important to the real\n\
  \world."

-- code = huffman (frequencies why)
-- encode code why
-- decode code it
-- decode code it == why
