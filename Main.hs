import Data.Char
import Debug.Trace

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y  | isDigit x && isDigit y = read (x:(y:[]))::Int | otherwise = 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((fst p1 - fst p2)^2 + (snd p1 - snd p2)^2)

fact2 :: Integer -> Integer
fact2 0 = 1
fact2 1 = 1
fact2 n = n * fact2 (n - 2)

fib :: Integer -> Integer
fib 1 = 1
fib 0 = 0
fib (-1) = 1
fib n 
	| n > 1 = fib (n - 1) + fib (n - 2) 
	| n < -1 = fib (n + 2) - fib (n + 1)



fib' :: Integer -> Integer
fib' n = helper n 1 1

helper :: Integer -> Integer -> Integer -> Integer
--helper 1 f1 f2 = f1
--helper 0 f1 f2 = 0
--helper (-1) f1 f2 | f2-f1 == 0 =  | otherwise =  f2-f1
helper n f1 f2
	| n == 1 = f1
	| n == 0 = 0
--	| n == -1 && f2 - f1 == 0 = helper (n-1) f1 (f1-f2)
	| n == -1 = f2 - f1
	| n > 1 = helper (n-1) f2 (f1+f2)
	| n < -1 = helper (n+1) f2 (f1-f2)


--func :: (Num a) => a -> a -> a -> a
--func n c b a = func (n-1) q b a
--	 where q= c+p-
--	                  
--
--f :: (Num a) => a -> a
--f k = func k 3 2 1



