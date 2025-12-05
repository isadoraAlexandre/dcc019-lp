soma [] = 0
soma (x:xs) = x + soma xs

produto [] = 1
produto (x:xs) = x * produto xs

conj [] = True
conj (x:xs) = x && conj xs

-- f [] = elemeto nçeutro
-- f (x:xs) = x op f xs

mfoldr f e [] = e
mfoldr f e (x:xs) = x `f` mfoldr f e xs

soma xs = mfoldr (+) 0 xs
produto = mfoldr (*) 1
conj = mfoldr (&&) True

--soma [1,2,3] = mfoldr (+) 0 1:[2,3]
-- = 1 + (mfoldr (+) 0 2:[3])
-- = 1 + (2 + (mfoldr (+) 0 3:[]))
-- = 1 + (2 + (3 + (mfoldr (+) 0 [])))
-- = 1 + (2 + (3 + (0)))

pot = mfoldr (^) 1
pow [2,3,4] = 
