{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] (_:_) = 0
exactMatches (_:_) [] = 0
exactMatches [] [] = 0
exactMatches (x:xs) (y:ys) = if x == y then 1 + exactMatches xs ys
                             else 0 + exactMatches xs ys

{- 
https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#v:id
>>> length $ filter id [True, True, False, True]
3
-}

exactMatches2 :: Code -> Code -> Int
exactMatches2 c1 c2 = length $ filter id $ zipWith (==) c1 c2 


-- Exercise 2 -----------------------------------------

-- For each peg in code, count how many times it occurs in colors
countColors :: Code -> [Int]
countColors code = [f code x | x <- colors]
                 where f codeList c = length $ filter (==c) codeList

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches code guess = length (filter (> 0) (zipWith min (countColors code) (countColors guess)))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove code guess = Move guess (exactMatches code guess) nonExactMatches
                   where 
                     nonExactMatches = (matches code guess) - (exactMatches code guess)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move code exactMatch nonexactMatch) guess = (exact == exactMatch) && (nonexact == nonexactMatch)
                                                        where
                                                          Move _ exact nonexact = getMove guess code


-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (\code -> isConsistent move code) codes 

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
