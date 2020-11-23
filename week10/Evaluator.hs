{-# LANGUAGE InstanceSigs #-}
module EvaluatorEnv where

infixl 6 :+:
infixl 7 :*:
infixr 1 :?:

data Expr
  =  Lit Integer    -- a literal
  |  Expr :+: Expr  -- addition
  |  Expr :*: Expr  -- multiplication
  |  Div Expr Expr  -- integer division
  |  Expr :?: Expr  -- non-deterministic choice
  |  Var String     -- a variable

evalA :: (Applicative f) => Expr -> f Integer
evalA (Lit i)      =  pure i
evalA (e1 :+: e2)  =  pure (+)  <*> evalA e1 <*> evalA e2
evalA (e1 :*: e2)  =  pure (*)  <*> evalA e1 <*> evalA e2
evalA (Div e1 e2)  =  pure div  <*> evalA e1 <*> evalA e2

----------
-- 10.1 --
----------

toss  ::  Expr
toss  =  Lit 0 :?: Lit 1

-- making a nondeterministic choice between the two arguments by returning all possible results
-- means we can just return xs ++ ys 
nonDet :: [a] -> [a] -> [a]
nonDet xs ys = xs ++ ys

evalN :: Expr -> [Integer]
evalN (Lit i) = [i]
evalN (e1 :+: e2) = [x + y |x <- (evalN e1), y <-  (evalN e2)] 
evalN (e1 :*: e2) = [x * y |x <- (evalN e1), y <-  (evalN e2)]
evalN (Div e1 e2) = [ x `div` y | x <- (evalN e1), y <- (evalN e2)]
evalN (e1 :?: e2) = nonDet (evalN e1) (evalN e2)
-- Tests:
t1 :: [Integer]
t1 = evalN toss
t2 :: [Integer]
t2 = evalN (toss :+: Lit 2 :*: toss)
t3 :: [Integer]
t3 = evalN (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: toss)))

------------
-- 10.2.1 --
------------

newtype Environ a = EN { fromEN :: [(String, Integer)] -> a }

instance Functor Environ where
  fmap :: (a -> b) -> Environ a -> Environ b
  fmap f (EN x) = EN $ \env -> f (x env) 

instance Applicative Environ where
  pure :: a -> Environ a
  pure x = EN $ \env -> x
  (<*>) :: Environ (a -> b) -> Environ a -> Environ b 
  EN f <*> EN x  = f . x
  
lookupVar :: String -> Environ Integer
lookupVar x = EN $ lookUp x

lookUp :: String -> [(String, Integer)] -> Integer
lookUp x = \z -> case z of
  [] -> 0
  ((var, val):xs) -> if var == y then val else lookUp y xs
    where y = x

evalR :: Expr -> Environ Integer
evalR (Lit i) = EN $ \_ -> i
evalR (e1 :+: e2) = pure (+)  <*> evalR e1 <*> evalR e2
evalR (e1 :*: e2) = pure (*)  <*> evalR e1 <*> evalR e2
evalR (Div e1 e2) = pure div  <*> evalR e1 <*> evalR e2
-- evalR (e1 :?: e2) = pure nonDet <*> evalR e1 <*> evalR e2
evalR (Var x) = EN $ lookUp x

-- Tests:
-- evalR (Var "a" :+: Lit 1) [("a", 4711), ("b", 0815)]
-- evalR (Var "a" :*: Var "b") [("a", 4711), ("b", 0815)]
-- evalR (Var "a" :*: Var "c") [("a", 4711), ("b", 0815)]

------------
-- 10.2.3 --
------------

newtype EnvND a = EnN { fromEnN :: [(String, Integer)] ->  [a] }
instance Functor EnvND where
  fmap :: (a -> b) -> EnvND a -> EnvND b
  fmap f (EnN x) = EnvND $ f x

instance Applicative EnvND where
  pure :: a -> EnvND a
  pure x = error "pure (Applicative EnvND): not yet implemented"
  (<*>) :: EnvND (a -> b) -> EnvND a -> EnvND b 
  EnN f <*> EnN xs  = error "<*> (Applicative EnvND): not yet implemented"

evalNR :: Expr -> EnvND Integer
evalNR = error "evalNR: not yet implemented"

