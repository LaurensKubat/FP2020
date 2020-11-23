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
fromList (x:xs) = cons x $ fromList xs

toList :: ListRef elem -> IO [elem]
toList x = error "toList: not yet implemented"

foreach :: ListRef a -> (a -> IO b) -> IO (ListRef b)
foreach = error "foreach: not yet implemented"
