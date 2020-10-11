module MapReduce where

-- Laurens Kubat
-- s4626249

import Data.Monoid
import Runs

reduce  ::  (Monoid m) => [m] -> m
reduce  =  foldr (<>) mempty 

-- We have the following functions of type Bool -> Bool -> Bool, according to hoogle
-- (&&), (||), implies, ==>, <==> ===>, 

newtype And = And Bool
instance Semigroup And where
    And x <> And y = And (x && y)
instance Monoid And where
    mempty = And True
    x `mappend` y = x <> y
-- reduce is also known under and

newtype Or = Or Bool
instance Semigroup Or where
    Or x <> Or y = Or (x || y)
instance Monoid Or where
    mempty = Or False
    x `mappend` y = x <> y
-- reduce is also known under or

-- ==> is the operator for boolean logic, thus same as Implies
-- ===> is also an implication operator
newtype Implies = Implies Bool
instance Semigroup Implies where
    Implies x <> Implies y = Implies $ not x || y
instance Monoid Implies where
    mempty = Implies True
    x` mappend` y = x <> y
-- reduce for a list of bools [b0..b] would by not b0 && b1 && ... || b

-- <==> is the operator for if and only if, thus named iff
newtype Iff = Iff Bool
instance Semigroup Iff where
    Iff x <> Iff y = Iff $ not x || y && not y || x
instance Monoid Iff where
    mempty = Iff True
    x `mappend` y = x <> y


newtype OrdList elem = Ord [elem] 

instance (Ord elem) => Semigroup (OrdList elem) where
    Ord (x:xs) <> Ord (y:ys)
      | x < y =  Ord $ x : xs <> (y:ys)
      | otherwise = Ord $ y : (x:xs) <> ys
instance (Ord elem) => Monoid (OrdList elem) where
    mempty = Ord []
    xs `mappend`  ys = xs <> ys
{-
A sorting algorithm using the Ordlist monoid would looks something as follows:
The typeconstraint Ord a should be included in the usage of
OrdList a and I would say that a type signature such as
sort :: (OrdList a) => a -> a
should be sufficient, but I encountered the error
expected kind ‘* -> Constraint’,but ‘Monoid OrdList’ has kind ‘Constraint’
So I am not sure why that was not sufficient, but this seems to work
-}
sort :: (Ord a) => OrdList a -> OrdList a
sort (Ord xs) = Ord $ reduce $ runs xs


{-
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldm :: (a -> a -> a) -> a -> ([a] -> a)
the main difference between foldr and foldl is in the function,
The main difference between foldm and foldr/l is in the function,
and return value. This is because foldm has to work in a top manner, thus
the function executed on all element has to have the same type arguments
and return value.
 -}

-- A topdown implementation of foldm
foldm :: (a -> a -> a) -> a -> ([a] -> a)
foldm f _ [x, y] = f x y
foldm f start [x] = f x start
foldm f start xs = f (foldm f start firstHalve)  (foldm f start secondHalve)
    where (firstHalve, secondHalve) = splitAt (length xs `div` 2) xs

-- A bottomup implementation of foldm
foldmb :: (a -> a -> a) -> a -> ([a] -> a)
foldmb _ start [] = start
foldmb f start [x] = f x start
foldmb f start (x:y:xs) =  f (f x y) (foldmb f start xs)


data Bit = O | I
type Carry = Bit

kpg :: (Bit, Bit) -> (Carry -> Carry)
kpg (O,  O  )  =  \ _c  -> O  -- kill
kpg (O,  I  )  =  \ c   -> c  -- propagate
kpg (I,  O  )  =  \ c   -> c  -- propagate
kpg (I,  I  )  =  \ _c  -> I  -- generate

data KPG  =  K | P | G

-- instance Semigroup KPG
-- instance Monoid KPG

-- apply :: KPG -> (Carry -> Carry)
