-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, xs) = toRevDigits n == xs

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
             [(1234, [4, 3, 2, 1]), (0, []), (-17, [])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (xs, ys) = doubleEveryOther xs == ys

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [ ([4, 9, 5, 5], [4, 18, 5, 10]), ([0, 0], [0, 0])
             , ([5, 9, 5], [5, 18, 5])
             ]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (xs, x) = sumDigits xs == x

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits
             [([10, 5, 18, 4], 19)]
           ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (x, b) = luhn x == b

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
             [(5594589764218858, True), (123456789765432, False)]
           ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, p1, p2, p3, ms) = (hanoi n p1 p2 p3) == ms

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi
             [ (2, "a", "b", "c", [("a", "c"), ("a", "b"), ("c", "b")])
             , (3, "a", "b", "c", [ ("a", "b"), ("a", "c"), ("b", "c")
                                  , ("a", "b")
                                  , ("c", "a"), ("c", "b"), ("a", "b")
                                  ])
             ]
           ]

-- (Opional Exercise 7) ------------------------------

testHanoi4 :: (Integer, Peg, Peg, Peg, Peg, [Move]) -> Bool
testHanoi4 (n, p1, p2, p3, p4, ms) = (hanoi4 n p1 p2 p3 p4) == ms

ex7Tests :: [Test]
ex7Tests = [ Test "hanoi4 test" testHanoi4
              [ (1, "a", "b", "c", "d", [("a", "b")])
              , (2, "a", "b", "c", "d", [("a", "c"), ("a", "b"), ("c", "b")])
              , (3, "a", "b", "c", "d", [ ("a", "c"), ("a", "d")
                                        , ("a", "b")
                                        , ("d", "b"), ("c", "b")
                                        ])
              ]
            ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
