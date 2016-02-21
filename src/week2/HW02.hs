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

-- Count how many times x occurs in ys
frequency :: Eq a => a -> [a] -> Int
frequency _ [] = 0
frequency x (y:ys)
    | x == y    = 1 + frequency x ys
    | otherwise = frequency x ys

-- Count how many times each color appears in the code
countColors :: Code -> [Int]
countColors c = 
    let 
        count x = frequency x c 
    in 
        map count colors

-- Zip up the color count of two codes
zipCountCodes :: Code -> Code -> [(Int, Int)]
zipCountCodes c1 c2 =
    let
        c1Count = countColors c1
        c2Count = countColors c2
    in
        zip c1Count c2Count

-- Find the minimum of each tuple in a list of tuples
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
        exactCount   = exactMatches secret guess
        regularCount = (matches secret guess) - exactCount
    in
        Move guess exactCount regularCount

-- Exercise 4 -----------------------------------------

-- A Code is consistent with a Move if it has the same
-- number of exact and regular matches with the guess
-- inside the Move as the guess had with the secret
-- ie. the Code could be the secret that generated the Move
isConsistent :: Move -> Code -> Bool
isConsistent m c =
    let
        m2                = getMove c (code m)
        consistentExact   = (exact m2) == (exact m)
        consistentRegular = (regular m2) == (regular m)
    in
        consistentExact && consistentRegular

-- Exercise 5 -----------------------------------------

-- Filter a list of Code to only contain those that are
-- consistent with the Move
filterCodes :: Move -> [Code] -> [Code]
filterCodes _ [] = []
filterCodes m (c:cs)
  | consistent = c : filterCodes m cs
  | otherwise  = filterCodes m cs
  where
    consistent = isConsistent m c

-- Exercise 6 -----------------------------------------

-- Generate all the Codes of a length n
allCodes :: Int -> [Code]
allCodes n
  | n <= 0 = []
  | n == 1 = map (:[]) colors
  | otherwise =
        let 
            -- For each code returned by allCodes n-1, prepend c
            addColor c = map (c:) (allCodes $ n-1)
        in 
            concatMap addColor colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = 
    let
        firstGuess = replicate (length secret) Red
        firstMove = getMove secret firstGuess
        possibleCodes = allCodes (length secret)
    in
        getMove secret firstGuess : []

generateMoves :: [Code] -> [Move] -> [Move]
generateMoves [] xs = xs
generateMoves ys xs = 

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
