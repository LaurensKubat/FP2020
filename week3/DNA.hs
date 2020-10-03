module DNA where

-- Laurens Kubat
-- s4626249

import Data.Maybe
import Data.List

-- Nucleobases or DNA-bases are the basic building blocks of
-- deoxyribonucleic acid (DNA).

data Base  =  A | C | G | T
  deriving (Eq, Ord)

-- Adenin (A), Cytosin (C), Guanin (G) und Thymin (T).

instance Show Base where
  showsPrec _ A  =  showChar 'A'
  showsPrec _ C  =  showChar 'C'
  showsPrec _ G  =  showChar 'G'
  showsPrec _ T  =  showChar 'T'

  showList  =  foldr (.) id . map shows

base :: Char -> Maybe Base
base 'A'  =  Just A
base 'C'  =  Just C
base 'G'  =  Just G
base 'T'  =  Just T
base _    =  Nothing

type DNA      =  [Base]
type Segment  =  [Base]

exampleDNA :: DNA
exampleDNA = [A, T, G, T, A, A, A, G, G, G, T, C, C, A, A, T, G, A]

-- some added test segments to test contains
exampleSegmentTrue1 :: DNA
exampleSegmentTrue1 = [A, T, G]

exampleSegmentTrue2 :: DNA
exampleSegmentTrue2 = [A, A, A, G, G, G]

exampleSegmentFalse :: DNA
exampleSegmentFalse = [T, T, T, T, T,T]

mm :: DNA
mm = mapMaybe base
  "ATGTAAAGGGTCCAATGACTGGAATTACTTCACAGCCCTGACACTGTGGAGAGATGGATA\
  \CCAGTTTAATTATGGACAGCTGAGAATAATCTGGGAGTCACCTGCTCTGGATATTTATCA\
  \TCATTATGAACTTCTCACCAGGCTGTGGCCCGAACACTTTCATAACGTCCCATTTGTGTT\
  \GGGCAGACATTATGATCTATACAGAACCTGCCTTGTCTGTCACCTTTGAATTTATGGATA\
  \GACAATTTAATAT\
  \GTGTTCCTGGCAGCAAAGATAATCATGGAGAGTGGAGAGAAACTAACCTTACCATTGATA\
  \GGGAAACTCTTGAAGTGTCAACTTCTCCATATTAAATCCAAGGACCAGCAGAGACGAGAA\
  \AATGAAAAGAAGATGGTAGAAGAAAGAACAAAATCAGAAAAAGACAAAGGAAAAGGGAAG\
  \TCTCCAAAGGAGAAGAAAGTTGCCAGTGCCAAGCCTGGGAAGGGAAAGAGCAAGGACCAG"

readDNA :: FilePath -> IO [Base]
readDNA path = do
  x <- readFile path
  return (mapMaybe base x)

contains :: Segment -> DNA -> Bool
contains _ [] = False
contains (x:xs) (y:ys)
  | subseg (x:xs) (y:ys) = True
  | otherwise = contains (x:xs) ys -- move the next base in the DNA string and check if that contains the subsequence

-- check whether the subsegment is present
subseg :: Segment -> DNA -> Bool
subseg (x:[]) (y:_) = x == y
subseg (x:xs) (y:[]) = False
subseg (x:xs) (y:ys) -- = x == y && subseg xs ys is this the same as the two guards due to lazy evaluation?
  | x == y = subseg xs ys
  | x /= y = False

-- we create all possible subsequences, we then filter all subsequences that contain anything else than an a
-- then we return the length of the longest subsequence
longestOnlyAs :: DNA -> Integer
longestOnlyAs xs = toInteger $ length $ maximum $ filter onlyAs (subsequences xs)

onlyAs :: DNA -> Bool
onlyAs [] = True
onlyAs (x:xs) = x == A && onlyAs xs 

longestAtMostTenAs :: DNA -> Integer
longestAtMostTenAs xs
  | longestAs > 10 = 10
  | otherwise = longestAs
    where longestAs = longestOnlyAs xs

-- If you want to test your code on a larger example, say within GHCi

-- dna <- readDNA "mm1.dna"
-- longestOnlyAs dna
