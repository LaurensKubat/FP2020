module Main where
import System.Random

data Colour = White | Silver | Green | Red | Orange | Pink | Yellow | Blue
  deriving (Show, Eq)
type Code = [Colour]

getColour :: Int -> Colour
getColour r
  | r == 1 = White
  | r == 2 = Silver
  | r == 3 = Green
  | r == 4 = Red
  | r == 5 = Orange
  | r == 6 = Pink
  | r == 7 = Yellow
  | r == 8 = Blue

getCode :: Int -> [Int] -> Code
getCode 1 (x:[]) = [getColour x]
getCode x (y:xs)= [getColour y] ++ getCode (x-1) xs

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
  deriving Eq

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
checkVictory [] = True
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

getGuess :: IO Code
getGuess = do parseGuess . words <$> getLine

gameLoop :: Code -> Int -> IO ()
gameLoop code tries = do
    putStrLn (show tries ++ " tries left\n")
    g <- getGuess
    let c = checkCode g 0 code
    -- TODO give a hint if the code is false.
    let won = checkVictory c
    if (won) 
      then 
        putStrLn "You won!"
      else
        if (not won && (tries <= 0))
          then
            putStrLn "You Lost!"
          else
            giveHint c;
            gameLoop code (tries - 1);

main :: IO ()
main = do
  a <- getStdRandom $ randomR (1, 8);
  b <- getStdRandom $ randomR (1, 8);
  c <- getStdRandom $ randomR (1, 8);
  d <- getStdRandom $ randomR (1, 8);
  let code = getCode 4 [a, b, c, d]
  putStrLn "I picked a random code word with 4 colours.\nPossible colours are White Silver Green Red Orange Pink Yellow Blue.\nTry to guess the secret code word,"
  putStrLn "Guess have to be entered as the following example: White White Red White"
  gameLoop code 12
  putStrLn "Goodbye"

  
