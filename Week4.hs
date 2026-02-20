-- Week 4: Tuples, Strings and Lists
-- Programming Exercises

import Data.Char

-- Type synonym for student marks
type StudentMark = (String, Int)

-- Test data provided in worksheet
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jovan", 55),
            ("Cynthia", 43), ("Elena", 44), ("Carol", 23), ("Sarah", 91),
            ("Lisa", 81), ("C-3PO", 23), ("R2-D2", 46), ("Leia", 78),
            ("Finn", 87), ("Rey", 25), ("Luke", 21), ("Han", 84)]

-- Helper function: marks (extracts marks from StudentMark list)
marks :: [StudentMark] -> [Int]
marks stmks = [mk | (_, mk) <- stmks]

-- ============================================================================
-- WORKED EXAMPLE 1: Sum Even Numbers Between
-- ============================================================================

-- Version 1: Using list comprehension (recommended)
sumEvenNumbersBetween :: Int -> Int -> Int
sumEvenNumbersBetween x y = sum [i | i <- [x .. y], even i]

-- Version 2: Recursive (commented out)
-- sumEvenNumbersBetween :: Int -> Int -> Int
-- sumEvenNumbersBetween x y
--   | x > y            = 0
--   | mod x 2 == 0     = x + sumEvenNumbersBetween (x + 2) y
--   | otherwise        = sumEvenNumbersBetween (x + 1) y

-- HOW IT WORKS (list comprehension version):
-- sumEvenNumbersBetween 5 8
--   → [i | i <- [5,6,7,8], even i]
--   → [6, 8]  (only even numbers)
--   → sum [6, 8]
--   → 14

-- EXPLANATION:
-- [i | i <- [x .. y], even i] means:
-- "Give me all numbers i from the range x to y where i is even"
-- The "even i" is a FILTER - only includes numbers that pass the test


-- ============================================================================
-- WORKED EXAMPLE 2: Average Mark
-- ============================================================================

averageMark :: [StudentMark] -> Float
averageMark [] = 0  -- Empty list returns 0
averageMark stmks = fromIntegral sumMarks / fromIntegral numberOfStudents
  where
    sumMarks = sum [mk | (_, mk) <- stmks]
    numberOfStudents = length stmks

-- HOW IT WORKS:
-- averageMark [("Stefan", 56), ("Anya", 73)]
--   → sumMarks = sum [56, 73] = 129
--   → numberOfStudents = length [...] = 2
--   → 129 / 2 = 64.5

-- EXPLANATION:
-- [mk | (_, mk) <- stmks] is TUPLE UNPACKING
-- Each element of stmks is a pair (name, mark)
-- We unpack it into (_, mk) - ignore name, capture mark
-- Results in list of just the marks: [56, 73]