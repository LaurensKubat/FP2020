-- ghc --make WordCount.hs
module Main where

import System.Environment
import System.IO as IO
import System.FilePath

countNewLines :: String -> Int
countNewLines [] = 0
countNewLines (x:xs) 
  | x == '\n' = 1 + countNewLines xs
  | otherwise = countNewLines xs
 
-- since a char is one byte, we just count each char
countBytes :: String -> Int
countBytes [] = 0
countBytes (_:xs) = 1 + countBytes xs

-- Wordcount for one file in implemented here, needs wildcard matching
main :: IO ()
main = do
  args <- getArgs
  let filename = unwords args
  handle <- openFile filename ReadMode  
  contents <- IO.hGetContents handle 
  -- Here we do things with contents to mimic the wordcount behaviour
  let newLines = countNewLines contents-- count the newlines
  let wordcount = length $ words contents -- count the words
  let bytecount = countBytes contents-- count the bytes
  putStrLn (show newLines ++ " " ++ show wordcount ++ " " ++ show bytecount ++ " " ++ filename)
  hClose handle 
