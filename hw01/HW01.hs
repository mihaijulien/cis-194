{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
    | n <= 0     = [] 
    | n < 10    = [n]
    | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toRevDigits n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []  = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : (2*y) : doubleEveryOther xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = if x < 10 then x + sumDigits xs 
                   else xSum +  sumDigits xs
                       where
                         xSum = sumDigits (toRevDigits x)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = sumDigits(doubleEveryOther(toDigits n)) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 src _ dest = [(src, dest)]
hanoi n src aux dest = hanoi (n - 1) src dest aux ++ [(src, dest)] ++ hanoi (n - 1) aux src dest
