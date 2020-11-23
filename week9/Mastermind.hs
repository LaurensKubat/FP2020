module Mastermind where

import System.Random

data Colour = White | Silver | Green | Red | Orange | Pink | Yellow | Blue
  deriving (Show, Eq)
type Code = [Colour]

getRandomColour :: Colour
getRandomColour
  | r == 1 = White
  | r == 2 = Silver
  | r == 3 = Green
  | r == 4 = Red
  | r == 5 = Orange
  | r == 6 = Pink
  | r == 7 = Yellow
  | r == 8 = Blue
    where r = randomRIO (1,8)

getCode :: Code
getCode = [getRandomColour, getRandomColour, getRandomColour, getRandomColour]

parseGuess :: [String] -> Code
parseGuess [] = []
parseGuess (x:xs)
  | x == show White   = White   : parseGuess xs
  | x == show Silver  = Silver  : parseGuess xs
  | x == show Green   = Green   : parseGuess xs
  | x == show Red     = Red     : parseGuess xs
  | x == show Orange  = Orange  : parseGuess xs
  | x == show Pink    = Pink    : parseGuess xs
  | x == show Yellow  = Yellow  : parseGuess xs
  | x == show Blue    = Blue    : parseGuess xs

-- Correctness shows the correctness of a pin, correct is both correct spot and colour
-- Spot means that only the spot in wrong
data Correctness = Correct | Spot | Wrong

(&&&) :: Correctness -> Correctness -> Correctness
x &&& y
  | x == Correct || y == Correct = Correct
  | x == Spot || y == Spot = Spot
  | otherwise = Wrong

-- check if colour is at index else check if colour is in code
checkPosition :: Colour -> Int -> Code -> Correctness
checkPosition x i (y:ys)
  | (y:ys) !! i == x = Correct
  | x == y = Spot
  | otherwise = Wrong &&& checkPosition x i ys


checkCode :: Code -> Int -> Code -> [Correctness]
checkCode [] _ _ = []
checkCode (x:xs) i ys = (checkPosition x i ys) : (checkCode xs (i+1) ys)

checkVictory :: [Correctness] -> Bool
checkVictory (x:xs)
  | x == Correct = True && checkVictory xs
  | otherwise = False

countCorrect :: [Correctness] -> Int
countCorrect (x:xs)
  | x == Correct = 1 + countCorrect xs
  | otherwise = countCorrect xs

countSpot :: [Correctness] -> Int
countSpot (x:xs)
  | x == Spot = 1 + countCorrect xs
  | otherwise = countCorrect xs

giveHint :: [Correctness] -> IO ()
giveHint c = do 
  putStrLn ("Incorrect\n" ++ show (countCorrect c) ++ " colour(s) in the correct position,\n" ++ show (countSpot c) ++ " colour(s) in the wrong position.\n")

tellTries :: Int -> IO ()
tellTries i = do
  putStrLn (show i ++ " tries left\n")

getGuess :: Code
getGuess = do
  guess <- getLine
  return $ parseGuess $ words guess

gameLoop :: Code -> Int -> IO Bool
gameLoop code tries = do
    putStrLn (show tries ++ " tries left\n")
    let g = getGuess
    let c = checkCode g 0 code
    won <- checkVictory c
    if (not won && (tries > 0))
      then
        giveHint c
      else
        return won
    return $ gameLoop code (tries - 1)


main :: IO ()
main = do
  let code = getCode
  putStrLn "I picked a random code word with 4 colours.\nPossible colours are White Silver Green Red Orange Pink Yellow Blue.\nTry to guess the secret code word,"
  let won = gameLoop code 12
  if (won)
    then
      putStrLn "Correct\n"
    else
      putStrLn ("No more tries, game over!\nThe code was " ++ show code)
  putStrLn "Goodbye"

  
