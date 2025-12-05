-- ======== 1 =========

data Nat = Zero | Suc Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Suc m)  n = Suc (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Suc Zero) n = n
mult (Suc m) n =  add (mult m n) (mult (Suc Zero) n)

leq :: Nat -> Nat -> Bool
leq Zero n = True
leq n Zero = False
leq (Suc n) (Suc m) = leq n m

-- ======== 2 =========

elem1 :: Eq a => a -> [a] -> Bool
elem1 x = foldr (\y acc -> (if x == y then True else False)) True

remdups :: Eq a => [a] -> [a]
remdups = foldr (\y acc -> if acc == [] then y : acc else (if y == head acc then acc else y : acc)) []

-- ======== 3 =========
-- a
data Queue a = Emptyq | Q [a] deriving Show

enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs) = Q (xs ++ [x])

dequeue :: Queue a -> (a, Queue a)
dequeue (Q (x:xs)) = (x, Q xs)

init1 :: [a] -> Queue a
init1 [] = Emptyq
init1 xs = Q xs

-- b
instance Functor Queue where
 fmap _ Emptyq = Emptyq
 fmap f (Q xs) = Q (map f xs)
 
instance Applicative Queue where
 pure x = Q [x]
 _ <*> Emptyq = Emptyq
 (Q xs) <*> (Q fs) = Q [f x | x <- xs, f <- fs]
