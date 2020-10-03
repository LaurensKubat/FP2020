
-- Laurens Kubat
-- s4626249

import Data.Char
    ( ord, chr, isDigit, isLower, isUpper, toLower, toUpper )

-- Exercise 2.2
(<:=) :: [a] ->(Int,a) ->[a]
(<:=) x (y, z ) = x

f1 :: [Int] -> Int -> Int -> Int
f1 x y z
  | z < 0 || z >= length x = -1
  | x !! z == y = z
  | otherwise = f1 x y (z+1)

f2 :: [Int] -> Int -> Int
f2 x y = f1 x y 0

f3 :: [Char] -> Int -> [Char]
f3 x y
  | y < 0 || y >= length x = x
  | isUpper (x !! y) = f3 (x <:= (y,toLower (x !! y))) (y+1)
  | isLower (x !! y) = f3 (x <:= (y,toUpper (x !! y))) (y+1)
  | otherwise = f3 x (y+1)

f4 :: [Char] -> [Char]
f4 x = f3 x 0

f5 :: [a] -> Int -> Int -> [a]
f5 x y z
  | y > z = x
  | y < 0 = x
  | z >= length x = x
  | otherwise = f5 (x <:= (z,x !! y) <:= (y,x !! z)) (y+1) (z-1)

f6 :: [a] -> [a]
f6 x = f5 x 0 (length x - 1)

-- Exercise 2.3

equal :: String -> String -> Bool
equal xs ys = map toLower xs == map toLower ys
-- equal x:xs y:ys = tolower x == toLower y && equal xs ys

isNumeral :: String -> Bool
isNumeral xs = and $ map isDigit xs

isBlank :: String -> Bool
isBlank "" = True
isBlank (x:"") = x == ' '
isBlank (x:xs) = x == ' ' && isBlank xs

-- We subtract 65 to make this work more easily in shift
fromDigit :: Char -> Int
fromDigit x = ord x - 65

-- we add 65 to make this work more easily in shift
toDigit :: Int -> Char
toDigit x = chr $ x + 65

-- Since we only need to shift capital letters to capital letters, we can just modulo by 65 and then add 65
-- to make sure that 'Z' shifts to a 'A' for shift 1 'Z'
-- We leave the spaces unshifted since they are clearly not part of the cipher
shift :: Int -> Char -> Char
shift x ' ' = ' '
shift x y =  toDigit $ (fromDigit y + x) `mod` 26

cipher :: String -> Int -> String
cipher xs y = map (shift y) xs

-- Trying cipher msg 19 gives us
-- "FABER EST SUAE QUISQUE FORTUNAE APPIUS CLAUDIUS CAECUS DICTUM ARCNUM EST NEUTRON" 
msg  ::  String
msg  =  "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ \
        \JHLJBZ KPJABT HYJUBT LZA ULBAYVU"

-- Exercise 2.4
-- We executed the following commands:
-- *Main> product [1..10]::Int
-- 3628800
-- *Main> product [1..20]::Int
-- 2432902008176640000
-- *Main> product [1..21]::Int
-- -4249290049419214848
-- *Main> product [1..65]::Int
-- -9223372036854775808
-- *Main> product [1..66]::Int
-- 0
-- *Main> product [1..10]::Integer
-- 3628800
-- *Main> product [1..20]::Integer
-- 2432902008176640000
-- *Main> product [1..21]::Integer
-- 51090942171709440000
-- *Main> product [1..65]::Integer
-- 8247650592082470666723170306785496252186258551345437492922123134388955774976000000000000000
-- *Main> product [1..66]::Integer
-- 544344939077443064003729240247842752644293064388798874532860126869671081148416000000000000000
-- As we can observe, line 84 suddenly becomes negative and line 88 becomes 0, which are both unexpected results
-- This looks like a stack overflow occurs. If we redo this using Integer, no stack overflow seems to occur.
-- *Main> product [1..513]::Integer
-- 178384966387702655136420255570089521725202458634512297458568958858597614139003885480175665322416465611953740818553975401736335873161945403804510547102861163917019186254939403859713981472996146412352788848758798802031402443586737978986848568689630136748436607445577470013480642063050029767935765986615732510598650663378810149429046610235414808285000901474773169732957665691254788704415715172934284805825531745026974360301621780212211934682240066900630411166639104218166471788229692758366484576570098336321592769557509965256638993939438361329712141512803067604188570937566146215898966737015680055471223244583858377798842385513136031307486244689284501071483585218271763071538699375195939786236534332529955450454971413964095369943469944775656284894286591952806808424919261776735435891960406921352733732533297625488709909512865671210090023629725317736260624959183556597212032549674556967498908440317136573572084672211175229686221290749494833380019061632515789910881106876369986928441620082474294832192283547240843551050323645693364814122821208768512000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- the above also produces no stackoverflow. According to the haskell documentation, Integer can represent arbitrarily large numbers, up to all the
-- machines memory

-- Exercise 2.5
swapInt :: (Int, Int) -> (Int, Int)
swapInt (x, y) = (y, x)

-- as we can see, upon changing the the type signature swap, the same implementation of swapInt still is valid
-- since swap with a and b as an Int is a valid usage, swapInt (a swap with Int in the type) is not needed
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- The difference between (Int, (Char, Bool)) and (Int, Char, Bool) is that the first is a tuple
-- containing an Int and a second tuple which in turn contains a Char and Bool, the second is a tuple
-- containing 3 items
unpack :: (Int, (Char, Bool)) -> (Int, Char, Bool)
unpack (x, (y, z)) = (x, y, z) 

-- Exercise 2.6 
-- (+ 4)
-- (+ 4) :: Num a => a -> a

-- div
-- div :: Integral a => a -> a -> a

-- div 7
-- div :: Integral a => a -> a

-- (div 7) 4
-- div :: Integral a => a

-- div (7 4)
-- div (7 4) :: (Integral a, Num t, Num (t -> a)) => a -> a

-- 7 ‘div‘ 4
-- 7 `div` 4 :: Integral a => a

-- + 3 7
-- the above is not well formed

-- (+) 3 7
-- (+) 3 7 :: Num a => a

-- (b, ’b’, "b")

-- (abs, ’abs’, "abs")

-- abs ◦ negate

-- (* 3) ◦ (+ 3)

-- (abs ◦ ) ◦ ( ◦ negate)
-- (div ◦ ) ◦ ( ◦ mod)