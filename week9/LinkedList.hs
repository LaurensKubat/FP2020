module LinkedList where

import Data.IORef

type ListRef elem = IORef (List elem)

data List elem = Nil | Cons elem (ListRef elem)

-- initiate an empty list
nil :: IO (ListRef elem)
nil =  newIORef Nil

-- cons attaches elem e to the head of list l
cons :: elem -> ListRef elem -> IO (ListRef elem)
cons e  l = newIORef $ Cons e l 

fromList :: [elem] -> IO (ListRef elem)
fromList [] = nil
fromList (x:xs) = do
    ref <- fromList xs
    cons x ref

toList :: ListRef elem -> IO [elem]
toList x = do 
    zs <- readIORef x
    toList' zs

toList' :: List elem -> IO [elem]
toList' Nil = return []
toList' (Cons x xs) = do
    ys <- readIORef xs
    zs <- toList' ys
    return $ x : zs

foreach :: ListRef a -> (a -> IO b) -> IO (ListRef b)
foreach xs f = do
    ys <- readIORef xs
    foreach' ys f

foreach' :: List a -> (a -> IO b) -> IO (ListRef b)
foreach' Nil _ = nil
foreach' (Cons x xs) f = do
    y <- f x
    ys <- foreach xs f
    cons y ys
