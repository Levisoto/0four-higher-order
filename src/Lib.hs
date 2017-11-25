module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' n = foldl (*) 1 $ map (\x -> x - 2) $ filter even n

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n = sum $ filter even $ takeWhile (/=1) $ iterate (\ x -> if (even x) then (div x 2) else (3*x+1)) n
---------------------------------------------------------------------------
---------------------------------------------------------------------------
  -- Problem 2: Construct a balanced Tree
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: Eq a => [a] -> Tree a
foldTree xs = foldr insertt Leaf xs

height :: Tree a -> Integer
height Leaf =  0
height (Node _ Leaf _ Leaf) = 0
height (Node _ x _ y) = 1 + max (height x) (height y)

insertt :: a -> Tree a -> Tree a 
insertt a Leaf = Node 0 Leaf a Leaf
insertt a (Node 0 Leaf x Leaf) = (Node 1 Leaf x (Node 0 Leaf a Leaf))
insertt b ((Node 1 Leaf x (Node 0 Leaf a Leaf))) = (Node 1 (Node 0 Leaf b Leaf) x (Node 0 Leaf a Leaf))
insertt a lis@(Node x left y right) 
  | (((iscomplete left) && not (iscomplete right)) || iscomplete lis) = newright
  | otherwise = newleft 
    where
      h x
        | iscomplete lis = x + 1
        | otherwise = x
      newright = (Node (h x) left y (insertt a right))
      newleft = (Node x (insertt a left) y right) 

-- isbalanced :: Tree a -> Bool
-- isbalanced Leaf = True
-- isbalanced (Node _ left _ right) 
--   | (abs $ (height left) - (height right)) <= 1 = True
--   | otherwise = False

iscomplete :: Tree a -> Bool
iscomplete Leaf = False
iscomplete (Node _ Leaf _ Leaf) = True
iscomplete (Node _ left _ right) = (iscomplete left) && (iscomplete right) && (height left == height right)

---------------------------------------------------------------------------
---------------------------------------------------------------------------
  -- Problem 3: Build a function that revice a list of Bool and ouput is Bool
  --
xor :: [Bool] -> Bool
xor list = foldr (\x y -> if (x && y) then (False) else (x || y)) False list

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f list = foldr (\x y -> (f x):y) [] list

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x y ->f y x) base $ reverse xs

---------------------------------------------------------------------------
---------------------------------------------------------------------------
  -- Problem 4: Finding Primes -> Sieve of Sundaram

sieveSundaram :: Integer -> [Integer]
sieveSundaram num = filter posibility [1..2*num+2]
  where
    posibility val = (val/=1) && odd val && (not $ existIn val)

existIn :: Integer -> Bool
existIn num 
  | elem True val = True
  | otherwise = False
  where
    val = map (elem num) $ generateTable (2*num + 2)

generateTable :: Integer -> [[Integer]]
generateTable num = [list i | i <- [1..num]]
  where
    list n = takeWhile (<(2*num+2)) [(2*n+1)^2,(2*n+1)^2+(4*n+2)..] 
