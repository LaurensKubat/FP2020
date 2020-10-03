-- Laurens Kubat s4626249


-- Exercise 1.1. Describe what each of the following Haskell functions mean:
-- f1 returns 6
f1 :: Int
f1 = 1 + 5

-- f2 return m if m < n else f2 returns n
f2 :: Int -> Int -> Int
f2 m n 
  | m < n = m
  | otherwise = n

-- f3 returns "" if n =< 0 else it returns s ++ f3 s (n-1)
-- thus it returns s n times 
f3 :: String -> Int -> String
f3 s n
  | n <= 0 = ""
  | otherwise = s ++ f3 s (n-1)

-- f4 returns x if y == 0  else it returns f4 y (x mod y)
-- which makes return the gcd through the it the euclidian algorithm
f4 :: Int -> Int -> Int
f4 x 0 = x
f4 x y = f4 y (x `mod` y)

-- f5 is called with a tuple as argument, fst gets the first item of the tuple, snd the second
-- f5 returns the sum of both
f5 :: (Int,Int) -> Int
f5 x = fst x + snd x

-- f6 swaps the first and second item of the tuple
f6 :: (a,b) -> (b,a)
f6 (a,b) = (b,a)

-- f7 swaps the given tuple in the argument twice by calling f6, thus returning the original tuple
f7 :: (a,a) -> (a,a)
f7 x = f6 (f6 x)

-- Excercise 1.2
-- e1 = 42  no reduction needed

-- e2 = 1 + 125 ∗ 8 / 10 - 59
         -------
-- e2 = 1 + 1000 / 10 - 59
--          ---------
-- e2 = 1 + 100 - 59
--      -------
-- e2 = 101 - 59
--      --------
-- e2 = 42

-- e3 = not True || True && False
--      --------
-- e3 = False || True && False
--      -------------
-- e3 = True && False
--      -------------
-- e3 = False

-- e4 = 1 + 2 == 6 - 3
--      -----
-- e4 = 3 == 6 - 3
--           -----
-- e4 = 3 == 3
--      ------
-- e4 = True

-- e5 = "1 + 2" == "6 - 3"
--      ------------------, since we are comparing two strings, this is just an evaluation of the ==
-- e5 = False

-- e6 = "1111 + 2222" == "1111" ++ " + " ++ "2222"
--                       -------------------------
-- e6 = "1111 + 2222" == "1111 + 2222"
--      ------------------------------
-- e6 = True

-- Exercise 1.3
type Person  =  (Name, Age, FavouriteCourse)

type Name             =  String
type Age              =  Integer
type FavouriteCourse  =  String

frits, peter, ralf, laurens :: Person
frits  =  ("Frits",  33,  "Algorithms and Data Structures")
peter  =  ("Peter",  57,  "Imperative Programming")
ralf   =  ("Ralf",   33,  "Functional Programming")
laurens = ("Laurens", 23, "Websec")

students   ::  [Person]
students   =  [frits, peter, ralf, laurens]

-- Why are there undescore in the pattern, the compiler seem to accept name as a valid function
age :: Person -> Age
age (_n, a, _c)  =  a

name :: Person -> Name
name (n, a, c) = n

favouriteCourse  :: Person -> FavouriteCourse
favouriteCourse (n, a, c) = c

showPerson :: Person -> String
showPerson (n, a, c) = n ++ " is " ++ show a ++ " years old and his favorite course is " ++ c

twins :: Person -> Person -> Bool
twins (n1, a1, c1) (n2, a2, c2) = a1 == a2

increaseAge :: Person -> Person
increaseAge (n, a, c) = (n, a + 1, c)

-- Exercise 1.8
data Shape
  =  Circle Double            -- radius
  |  Square Double            -- length
  |  Rectangle Double Double  -- length and width
  deriving (Show)

showShape :: Shape -> String
showShape (Circle r)       =  "circle of radius " ++ show r
showShape (Square l)       =  "square of length " ++ show l
showShape (Rectangle l w)  =  "rectangle of length " ++ show l
                                ++ " and width " ++ show w

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Square l) = l * l
area (Rectangle l w) = l * w

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square l) = 4 * l
perimeter (Rectangle l w) = 2 * l + 2 * w

center :: Shape -> (Double, Double)  -- x- and y-coordinates
center (Circle r) = (r/2, r/2) -- center coords are at x/2 and y/2
center (Square l) = (l/2, l/2)
center (Rectangle l w) = (l/2, w/2)

boundingBox  :: Shape -> (Double, Double)  -- width and height
boundingBox (Circle r) = (r, r) -- returns a box of r width and r height
boundingBox (Square l) = (l, l) -- the bounding box is the square, thus l, l
boundingBox (Rectangle l w) = (l, w) --the boundingbox of a rectangle is the rectangle

-- Exercise 1.14
triangle :: Int -> String
triangle l = triang l 1

-- The max amount of * in the triangle is 1 + 2 * l
-- thus we need l - i spaces
-- the first argument gives total amount of lines, i gives the current line
triang :: Int -> Int -> String
triang l i 
  | i < l = concat $ replicate (l - i) " " ++ replicate  i "*"  ++ replicate (l - i) " " ++ ["\n"] ++ [triang l (i + 1)]
  | i >= l = concat $ replicate (l - i) " " ++ replicate  i "*" ++ replicate (l - i) " " ++ ["\n"]

-- print the tree using the triangle function and a recursive call
tannenbaum :: Int -> String
tannenbaum 0 = ""
tannenbaum i = tannenbaum (i - 1) ++ triangle i