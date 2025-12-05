data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert e Empty = (Node e Empty Empty)
insert e (Node x left right)
 | e < x = Node x (insert e left) right
 | e > x = Node x left (insert e right)
 | otherwise = Node x left right

data Ntree a = Nnode a [Ntree a] deriving Show

altura :: Ntree a -> Int
altura (Nnode e []) = 1
altura (Nnode e f) = 1 + maximum (map altura f)

fraiz :: Ntree a -> Int
fraiz (Nnode _ f) = length f

media (x:xs) = sum (x:xs) / (fromIntegral $ length (x:xs))

lerValores :: Int -> IO [Double]
lerValores 0 = return []
lerValores n = do
 valor <- getLine
 let num = read valor :: Double
 resto <- lerValores (n-1)
 return (num : resto)

soma :: Int -> IO ()
soma n = do
 sum <- lerValores n
 putStrLn ("soma deu " ++ show (media sum))
 

pal :: IO()
pal = do
 n <- getLine
 if n == reverse n then putStrLn "sim" else putStrLn "nao"
 
