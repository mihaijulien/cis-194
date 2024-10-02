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
testToRevDigits (n, ns) = toRevDigits n == ns

ex2Tests :: [Test]
ex2Tests = [Test "toRevDigits tests" testToRevDigits
            [(1234, [4,3,2,1]), (1,[1]), (0, []), (-17, [])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (xs, ys) = doubleEveryOther xs == ys

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther" testDoubleEveryOther
              [([], []), ([1], [1]), ([1, 2], [1, 4]), ([1,2,3], [1,4,3])]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (xs, y) = sumDigits xs == y

ex4Tests :: [Test]
ex4Tests = [Test "sumDigits" testSumDigits
            [([], 0), ([1], 1), ([1,2], 3), ([1,23,4], 10)]
           ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, b) = luhn n == b

ex5Tests :: [Test]
ex5Tests = [ Test "luhn" testLuhn
            [(378662904948057, True), (1234567898765432, False)]
           ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, String, String, String, [(String, String)]) -> Bool
testHanoi (n, a, b, c, ms) = hanoi n a b c == ms

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi" testHanoi
            [
              (0, "a", "b", "c", []),
              (1, "a", "b", "c", [("a", "c")]),
              (2, "a", "b", "c", [("a", "b"), ("a", "c"), ("b", "c")]),
              (3, "a", "b", "c", [("a", "c"), ("a", "b"), ("c", "b"), ("a", "c"), ("b", "a"), ("b","c"), ("a", "c")])
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
