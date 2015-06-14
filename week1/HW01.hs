{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n 
    | n <= 0    = []
    | otherwise = (lastDigit n) : (toRevDigits (dropLastDigit n))

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x:[])   = [x]
doubleEveryOther (x:y:ys) = x : y*2 : doubleEveryOther ys

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toRevDigits

-- Exercise 5 -----------------------------------------

doubleEveryOtherDigits :: Integer -> [Integer]
doubleEveryOtherDigits n = doubleEveryOther (toRevDigits n)

sumDoubleOtherDigits :: Integer -> Integer
sumDoubleOtherDigits n = sumDigits (doubleEveryOtherDigits n)

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = ((sumDoubleOtherDigits n) `mod` 10) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

-- Move n disks from the first peg, to the second, without putting
-- a larger disk on a smaller one
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3
    | n <= 0    = []
    | otherwise =
        let
            step1 = hanoi (n-1) p1 p3 p2
            step2 = [(p1, p2)]
            step3 = hanoi (n-1) p3 p2 p1
        in
            step1 ++ step2 ++ step3

-- The same as hanoi, however, we now have an extra peg to utilise
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n p1 p2 p3 p4
    | n <= 0    = []
    | otherwise =
        let
            (half1, half2) = segment n
            step1          = hanoi4 half1 p1 p3 p2 p4
            step2          = hanoi half2 p1 p4 p2
            step3          = [(p1, p2)]
            step4          = hanoi half2 p4 p2 p1
            step5          = hanoi4 half1 p3 p2 p1 p4
        in
            step1 ++ step2 ++ step3 ++ step4 ++ step5

-- Segment n into two 'halves' such that half1 + half2 = n - 1
-- We want half1 to be larger that half2, because half1 disks are
-- moved in step1 and step2 of hanoi4, and we have move pegs as
-- temporary storage in those steps
segment :: Integer -> (Integer, Integer)
segment n =
        let
            half2 = (n `div` 3)
        in
            (n-half2-1, half2)
