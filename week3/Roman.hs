module Roman where

-- Laurens Kubat
-- s4626249

data RD    = I | V | X | L | C | D | M
type Roman = [RD]

instance Show RD where
  showsPrec _ I  =  showChar 'I'
  showsPrec _ V  =  showChar 'V'
  showsPrec _ X  =  showChar 'X'
  showsPrec _ L  =  showChar 'L'
  showsPrec _ C  =  showChar 'C'
  showsPrec _ D  =  showChar 'D'
  showsPrec _ M  =  showChar 'M'
  showList  =  foldr (.) id . map shows




romanToInt :: Roman -> Int
romanToInt [] = 0 
-- We hardcode the abbreviations
romanToInt (C:M:xs) = 900 + romanToInt xs
romanToInt (C:D:xs) = 400 + romanToInt xs
romanToInt (X:C:xs) = 90 + romanToInt xs
romanToInt (X:L:xs) = 40 + romanToInt xs
romanToInt (I:X:xs) = 9 + romanToInt xs
romanToInt (I:V:xs) = 4 + romanToInt xs
romanToInt (M:xs) = 1000 + romanToInt xs
romanToInt (D:xs) = 500 + romanToInt xs
romanToInt (C:xs) = 100 + romanToInt xs
romanToInt (L:xs) = 50 + romanToInt xs
romanToInt (X:xs) = 10 + romanToInt xs
romanToInt (V:xs) = 5 + romanToInt xs
romanToInt (I:xs) = 1 + romanToInt xs

intToRoman :: Int -> Roman
intToRoman 0 = []
intToRoman x 
  | x >= 1000 = M : intToRoman (x - 1000)
  | x >= 900 = C : M : intToRoman (x - 900)
  | x >= 500 = D : intToRoman (x - 500)
  | x >= 400 = C : D : intToRoman (x - 400)
  | x >= 100 = C : intToRoman (x - 100)
  | x >= 90 = X:C: intToRoman (x - 90)
  | x >= 50 = L : intToRoman (x - 50)
  | x >= 40 = X : L : intToRoman (x - 40)
  | x >= 10 = X : intToRoman (x - 10)
  | x >= 9 = I : X : intToRoman (x - 9)
  | x >= 5 = V : intToRoman (x - 5)
  | x >= 4 = I : V : intToRoman (x - 4)
  | x >= 1 = I : intToRoman (x - 1)
