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
