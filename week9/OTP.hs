-- ghc --make OTP.hs
module Main where

import System.Random
import System.Environment
import System.IO
import Data.Bits
import Data.Char

xorenc :: Char -> Int -> Char
xorenc a b = chr $ (ord a + b) `mod` 128 + 64

xordec :: Char -> Int -> Char
xordec a b = chr $ (ord a - b) `mod` 128 + 64

encrypt :: String -> [Int] -> String
encrypt (x:xs) (y:ys)
  | ord x < 32 = x : encrypt xs ys
  | otherwise =  xorenc x y : encrypt xs ys
encrypt _ [] = []
encrypt [] _ = []

decrypt :: String -> [Int] -> String
decrypt (x:xs) (y:ys)
  | ord x < 32 = x : decrypt xs ys
  | otherwise =  xordec x y : decrypt xs ys
decrypt _ [] = []
decrypt [] _ = []

getRnd :: IO Int
getRnd = getStdRandom (randomR (1,128))

main :: IO ()
main = do
  setStdGen (mkStdGen 4711)
  -- since encrypt and decrypt is the same action, we can just
  (arg1 : arg2 : arg3 : _) <- getArgs
  handle <- openFile arg2 ReadMode
  contents <- hGetContents handle
  rs <- sequence $ replicate (length contents) getRnd
  let res = if arg1 == "encrypt" then encrypt contents rs else decrypt contents rs
  writeFile arg3 res
