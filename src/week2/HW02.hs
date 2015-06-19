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
data Move = Move  { code :: Code
                  , exact :: Int
                  , regular :: Int
                  } deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] _          = 0
exactMatches _ []          = 0
exactMatches (x:xs) (y:ys)
    | x == y    = 1 + exactMatches xs ys
    | otherwise = exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
frequency :: Eq a => a -> [a] -> Int
frequency _ [] = 0
frequency x (y:ys)
    | x == y    = 1 + frequency x ys
    | otherwise = frequency x ys

-- Count how many times each color appears in the code
countColors :: Code -> [Int]
countColors x = [frequency c x | c <- colors]

-- Zip up the color count of two codes
zipCountCodes :: Code -> Code -> [(Int, Int)]
zipCountCodes c1 c2 =
    let
        c1Count = countColors c1
        c2Count = countColors c2
    in
        zip c1Count c2Count

-- Find the minimum of each tuple in a list of touples
mapTupleMin :: [(Int, Int)] -> [Int]
mapTupleMin [] = []
mapTupleMin ((x,y):zs) = min x y : mapTupleMin zs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 =
    let
        zippedCounts = zipCountCodes c1 c2
        mins         = mapTupleMin zippedCounts
    in
        sum mins

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess =
    let
        exact   = exactMatches secret guess
        regular = (matches secret guess) - exact
    in
        Move guess exact regular

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move code =
    let
        guess = code move
        guessExact = exact move
        guessRegular = regular move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
