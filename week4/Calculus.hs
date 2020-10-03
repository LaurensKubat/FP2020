module Calculus where

-- Laurens Kubat
-- s4626249
data Primitive
  =  Sin  -- trigonometric: sine
  |  Cos  -- cosine
  |  Exp  -- exponential
  deriving (Show)

infixl 6 :+:
infixl 7 :*:
infixr 9 :.:

data Function
  =  Const Rational         -- constant function
  |  Id                     -- identity
  |  Prim Primitive         -- primitive function
  |  Function :+: Function  -- addition of functions
  |  Function :*: Function  -- multiplication of functions
  |  Function :.: Function  -- composition of functions
  deriving (Show)



apply :: Function -> (Double -> Double)
apply (Const x) _ = fromRational x
apply Id x = x
apply (Prim Sin) x = sin x
apply (Prim Cos) x = cos x
apply (Prim Exp) x = exp x
apply (y :+: z) x  = apply y x + apply z x
apply (y :*: z) x  = apply y x * apply z x
apply (y :.: z) x  = apply y (apply z x)

derive   :: Function -> Function
derive (Const _) = (Const 0)
derive Id = (Const 1)
derive (Prim Sin) = (Prim Cos)
derive (Prim Cos) = (Prim Sin :*: (Const (-1)))
derive (Prim Exp) = (Prim Exp)
derive (x :+: y) = derive x :+: derive y
derive (x :*: y) = derive x :*: y :+: derive y :*: x
derive (x :.: y) = derive (x) :.: y :*: derive y

testExpr :: Function
testExpr = (Const 2) :*: Id :*: (Prim Exp)

simplify :: Function -> Function
simplify (Const (0) :*: _) = Const (0)
simplify (_ :*: Const (0)) = Const (0)
simplify (Const (0) :+: x) = simplify x
simplify (x :+: Const (0)) = simplify x
simplify (Const (1) :*: x) = simplify x
simplify (x :*: Const (1)) = simplify x
simplify (Const (1) :.: x) = simplify x
simplify (x :.: Const (1)) = simplify x
simplify (Id :.: x) = simplify x -- not sure if this simplification rule holds, but i assume it does
simplify (x :.: Id) = simplify x -- since f(x) . g(x) = f(g(x)) and if f(x) = x then f(g(x)) = g(x)
simplify (x :*: y) = simplify x :*: simplify y
simplify (x :+: y) = simplify x :+: simplify y
simplify (x :.: y) = simplify x :.: simplify y
simplify x = x
