module Hardware where

-- Laurens Kubat
-- s4626249

data Bit = O | I
  deriving (Eq, Ord, Show)

infixr 3 <&&>
(<&&>) :: Bit -> Bit -> Bit
O <&&> _  =  O
I <&&> b  =  b

infixr 2 <||>
(<||>) :: Bit -> Bit -> Bit
O <||> b  =  b
I <||> _  =  I

infixr 4 ><
(><) :: Bit -> Bit -> Bit
O >< O  =  O
O >< I  =  I
I >< O  =  I
I >< I  =  O

mapr :: ((a, state) -> (b, state)) -> (([a], state) -> ([b], state))
-- for mapr, we want to take the function of the first argument, and apply it to every item in [a] and carry over the state between calls to f
mapr f ([], state) = ([], state)
mapr f (x:xs, state) = (r : res, s)
  where
    (res, st) = mapr f (xs, state)
    (r, s) = f (x, st)
  
type Carry  =  Bit

halfAdder :: (Bit, Bit) -> (Bit, Carry)
halfAdder (O, O) = (O, O)
halfAdder (O ,I) = (I, O)
halfAdder (I, O) = (I, O)
halfAdder (I, I) = (O, I)

-- We implement the fullAdder using to half adders and the or between the carry outputs
fullAdder :: ((Bit, Bit), Carry) -> (Bit, Carry)
fullAdder ((a, b), cin) = (s, cout)
  where
    (res, c) = halfAdder (a, b)
    (s, co) = halfAdder (res, cin)
    cout = c <||> co

-- hardcoding would look something like this
-- where having carry 0 gives the same result as the halfadder
-- fullAdder (x, O) = halfAdder x
-- fullAdder ((O, O), I) = (I, O)
-- fullAdder ((O, I), I) = (O, I)
-- fullAdder ((I, O), I) = (O, I)
-- fullAdder ((I, I), I) = (I, I)

-- we zip the inputs together to create the tuples needed, then we use mapr and the fullAdder function create the output
-- if the carry bit is one after the addition, we prepend it to the result, else we return the result
-- depending on the implementation, one could choice to ignore the cout but give some sort of error
-- if cout == I. But i choose to prepend it
rippleCarryAdder :: [Bit] -> [Bit] -> [Bit]
rippleCarryAdder xs ys
  | cout == I = cout : res
  | otherwise = res
  where
    zs = zip xs ys
    (res, cout) = mapr fullAdder (zs, O)

testX :: [Bit]
testX = [O, O, I, I]

testY :: [Bit]
testY = [I, I, O, O]

testZ :: [Bit]
testZ = [I, I, I, I]

testResult1 :: [Bit]
testResult1 = rippleCarryAdder testX testY
-- result should be [I, I, I, I]

testResult2 :: [Bit]
testResult2 = rippleCarryAdder testY testZ
-- result should be [I, I, O, I, I]


