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

-- mapr :: ((a, state) -> (b, state)) -> (([a], state) -> ([b], state))

type Carry  =  Bit

-- halfAdder :: (Bit, Bit) -> (Bit, Carry)
-- fullAdder :: ((Bit, Bit), Carry) -> (Bit, Carry)
-- rippleCarryAdder :: [Bit] -> [Bit] -> [Bit]
