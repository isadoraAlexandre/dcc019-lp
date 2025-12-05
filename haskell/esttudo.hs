-- conteudo 1 --
ex1 = \x -> x -- retorna o proprio x

dobro = \x -> 2*x -- usa parentese tem q usar a seta
dobro1 x = 2*x

soma123 x y = x+y
somat (x,y) = x + y

-- estrutura de decisão: if () then () else ()
fat n = if n==0 then 1 else n*fat(n-1)

-- casamento de padroes
fat1 0 = 1
fat1 n = n*fat(n-1)

-- casamento de padrao com e
e True True = True
e True False = False
e False True = False
e False False = False

-- invertendo o casamento de padrao -> da erro `_´
-- fat2 n = n*fat(n-1)
-- fat2 0 = 1

-- defunicao usando guardas
fat3 :: Int -> Int
fat3 n
  | n == 0    = 1
  | otherwise = n * fat3 (n - 1)

-- fibonacci com if else
fib1 n = if n == 1 || n==0 then n else fib1(n-1) + fib1(n-2)

-- casamento de padrao
fib2 0 = 0
fib2 1 = 1
fib2 n = fib1(n-1) + fib1(n-2)

-- guardas
fib3 :: Int -> Int
fib3 n
 | n == 0 || n == 1 = n
 | otherwise = fib3(n-1) + fib3(n-2)

-- concatenar lista
--mconcat :: [a] -> [a] -> [a]
--mconcat [] ys = ys
--mconcat (x:xs) ys = x : mconcat xs ys

--indexação
mindex (x:xs) 0 = x
mindex (x:xs) n = mindex xs (n-1)

--tamanho da lista
mlen [] = 0
mlen (x:xs) = 1 + mlen xs

--retorna cabeca da lista
mhead (x:xs) = x

--n primeiros itens
mtake 0 _ = []
mtake n [] = []
mtake n (x:xs) = x : (mtake (n-1) xs)

--cauda da lista
mtail (x:xs) = xs

--reverte a lista
mrev [] = []
mrev (x:xs) = mrev (xs) ++ [x]

--remover n elementos da lista
mdrop 0 xs = xs
mdrop n [] = []
mdrop n (x:xs) = (mdrop (n-1) xs)

--função que dada duas listas, retorna uma lista formados por um elemento
mzip _ [] = []
mzip [] _ = []
mzip (x:xs) (y:ys) = (x,y) : mzip xs ys

--função que intercala duas listas (merge
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y :(merge xs ys)

--função que dado um elemento x e uma lista l, insere x entre os elementos de l
intersp _ [] = []
intersp _ [x] = [x]
intersp a (x:xs) = x : a : intersp a xs

--divide lista
mixHalf [] = ([], [])
mixHalf [x] = ([x],[])
mixHalf (x:y:xs) = let (as, bs) = mixHalf xs in (x:as, y:bs)

-- =========== e2 funções de prdem superior ============
--a)
somavet [] = 0
somavet (x:xs) = x + (somavet xs)

--b)
prod [] = 1
prod (x:xs) = x * (prod xs)

--c) n sei oq é conjugação de elementos
conj [] = True
conj (x:xs) = x && conj xs

--foldr f z (x:xs) = f x (foldr f z xs)
soma2 xs = foldr (+) 0 xs
prod2 xs = foldr (*) 1 xs
conj2 xs = foldr (&&) True xs

map2 f = foldr (\x acc -> f x : acc) []
filter2 p = foldr (\x acc -> if p x then x : acc else acc) []

map' f = foldl (\acc x -> acc ++ [f x]) []

--bit2int = foldl (\acc x -> acc * 2 + x) 0

replace x y = foldr (\sgsg acc -> (if sgsg == x then y else sgsg) : acc) []

count :: Eq a => [a] -> [(Integer, a)]
count = foldr update []
  where
    update x [] = [(1, x)]
    update x ((n,y) : ys)
      | x == y    = (n+1, y) : ys
      | otherwise = (n,y) : update x ys
      
mlength xs = sum (map (\_ -> 1) xs)

-- =========== e3 tipos sla ===============
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show

data Person = Person String Int deriving Show
idade (Person _ age) = age
nome (Person name _) = name

data Pessoa = Pessoa {amam :: String, age :: Int} deriving Show

data Car = Car {brand :: String, model :: String, year :: Int} deriving Show
marca (Car m _ _) = m

--pilha inteirods
data Stackint = Stackint [Int] deriving Show

pushInt :: Stackint -> Int -> Stackint
pushInt (Stackint stk) n = Stackint (n : stk)
popInt :: Stackint -> (Int, Stackint)
popInt (Stackint (x:xs)) = (x, Stackint xs)

--pilha genericos
data Stack a = Stack [a] deriving Show

push :: Stack a -> a -> Stack a
push(Stack xs) x = Stack (x:xs)
pop :: Stack a -> (a, Stack a)
pop (Stack (x:xs)) = (x, Stack xs)

--arvore
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

search :: (Num a, Ord a) => a -> Tree a -> Bool
search e Empty = False
search e (Node x left right)
 | e == x = True
 | e < x = search e left
 | otherwise = search e right

--insert :: a -> Tree a -> Tree a
--insert e Empty = (Node e Empty Empty)
--insert e (Node x left right)
-- | e == x = Node x left right
-- | e < x = Node x (insert e left) right
-- | otherwise = Node x left (insert e right)
 
data Formula = Var Char | Falso | Verdadeiro | Not Formula | End Formula | Or Formula | Impl Formula Formula | Iff Formula Formula deriving (Show, Eq)

-- ========== e4 io ========================================
hello :: IO()
hello = putStrLn "Holl"

hello2 :: IO()
hello2 = do 
 putStrLn "digt nome" 
 name <- getLine 
 putStrLn $ "wso " ++ name ++ " ;-;"
 
lennome :: IO()
lennome = do
 name <- getLine
 let tam = length name
 putStrLn $ show tam
 
media (x:xs) = sum (x:xs) / (fromIntegral $ length (x:xs))

lerValores :: Int -> IO [Double]
lerValores 0 = return []
lerValores n = do
 valor <- getLine
 resto <- lerValores (n-1)
 return (num : resto)

soma :: Int -> IO ()
soma n = do
 valores <- lerValores n
 putStrLn $ "media " ++ show (media valores)
 
-- reverseWords = unwords . map reverse . reverse . words
-- 
-- rev :: IO()
-- rev = do 
--  fraze <- getLine
 
pali :: IO()
pali = do
 entrada <- getLine
 let norm = filter (/= ' ') entrada
 if norm == reverse norm then putStrLn "sim" else putStrLn "nao" 

-- ============ e5 sla oq eh tbm ==============
pop2 :: Stack a -> (a, Stack a)
pop2 (Stack (x:xs)) = (x, Stack xs)

push2 :: a -> Stack a -> ((), Stack a)
push2 x (Stack xs) = ((), Stack (x:xs))

foo :: Stack Int -> ((), Stack Int)
foo s = let
 ((), s1) = push2 3 s
 (a, s2) = pop2 s1 
 in push2 5 s2  
 

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv a b = Just (a `div` b)

t = do
 x <- Just 3
 y <- Just 4
 z <- Just 5
 return (x+y+z)

f2' = do
 x <- Just 3
 y <- Just "vsf"
 return  (show x ++ y)

-- ========== functor ==========
data Maybe2 a = Just2 a | Nothing2 deriving Show

instance Functor Maybe2 where
 fmap func (Just2 a) = Just2 (func a)
 fmap func Nothing2 = Nothing2
 
data Tree2 a = Tip a | Branch (Tree2 a) (Tree2 a) deriving Show

instance Functor Tree2 where
 fmap f (Tip a) = Tip (f a)
 fmap f (Branch left right) = Branch (fmap f left) (fmap f right)
 
-- ========== apllicative =============

instance Applicative Maybe2 where
 pure = Just2
-- Just2 f <*> (Just2 j) = Just2 (f j)
 Just2 f <*> j = fmap f j
 Nothing2 <*> j = Nothing2

instance Applicative Tree2 where
 pure = Tip
 Tip f <*> t = fmap f t
 Branch left right <*> t = Branch (left <*> t) (right <*> t)
 
-- ========== monad =========

half x = if even x then Just2 (x `div` 2) else Nothing2

instance Monad Maybe2 where
 Nothing2 >>= f = Nothing2
 Just2 val >>= f = f val

g x 
 | x == 4 = (Tip 99)
 | otherwise = Branch (Tip (x*2)) (Tip (x*4))

instance Monad Tree2 where
 Tip a >>= f = f a 
 Branch left right >>= f = Branch (left >>= f) (right >>= f)
 
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

insert' x (Node y l r)
        | x < y     = Node y (insert' x l) r
        | x > y     = Node y l (insert' x r)
        | otherwise = Node y l r 


insertBST x = do
     t <- get
     put (insert' x t)

remove x = do
     t <- getput (delete x t)
     where 
          delete _ Empty = Empty
          delete x (Node y l r)
               | x < y     = Node y (delete x l) r
               | x > y     = Node y l (delete x r)
               | otherwise = removeNode (Node y l r)
     
         removeNode (Node _ Empty r) = r
          removeNode (Node _ l Empty) = l
          removeNode (Node _ l r)     = Node minRight l (delete minRight r)
               where minRight = leftmost r

          leftmost (Node y Empty _) = y
          leftmost (Node _ l _)     = leftmost l
