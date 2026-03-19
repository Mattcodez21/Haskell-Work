-- Week 5: List Patterns and Recursion
 
type StudentMark = (String, Int)
 
testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]
 
-- Worked Example 1: Count Spaces
countSpaces :: String -> Int
countSpaces "" = 0
countSpaces (x : xs)
  | x == ' '  = 1 + countSpaces xs
  | otherwise = countSpaces xs
 
-- Worked Example 2: Merge Lists
mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys
 
-- Exercise 1: Head Plus One
headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x:xs) = x + 1
 
-- Exercise 2: Duplicate Head
duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x : x : xs
 
-- Exercise 3: Rotate
rotate :: [a] -> [a]
rotate [] = []
rotate [x] = [x]
rotate (x:y:xs) = y : x : xs
 
-- Exercise 4: List Length
listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs
 
-- Exercise 5: Multiply All
multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs
 
-- Exercise 6: And All
andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs
 
-- Exercise 7: Or All
orAll :: [Bool] -> Bool
orAll [] = False
orAll (x:xs) = x || orAll xs
 
-- Exercise 8: Count Integers
countIntegers :: Int -> [Int] -> Int
countIntegers n [] = 0
countIntegers n (x:xs)
  | n == x    = 1 + countIntegers n xs
  | otherwise = countIntegers n xs
 
-- Exercise 9: Remove All
removeAll :: Int -> [Int] -> [Int]
removeAll n [] = []
removeAll n (x:xs)
  | n == x    = removeAll n xs
  | otherwise = x : removeAll n xs
 
-- Exercise 10: Remove All But First
removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst n [] = []
removeAllButFirst n (x:xs)
  | n == x    = x : removeAll n xs
  | otherwise = x : removeAllButFirst n xs
 
-- Exercise 11: List Marks
listMarks :: String -> [StudentMark] -> [Int]
listMarks name [] = []
listMarks name ((studentName, mark):rest)
  | name == studentName = mark : listMarks name rest
  | otherwise           = listMarks name rest
 
-- Exercise 12: Sorted
sorted :: [Int] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs)
  | x <= y    = sorted (y:xs)
  | otherwise = False
 
-- Exercise 13: Prefix
prefix :: [Int] -> [Int] -> Bool
prefix [] ys = True
prefix xs [] = False
prefix (x:xs) (y:ys)
  | x == y    = prefix xs ys
  | otherwise = False
 
-- Exercise 14: Subsequence
subSequence :: [Int] -> [Int] -> Bool
subSequence xs [] = False
subSequence xs ys
  | prefix xs ys = True
  | otherwise    = subSequence xs (tail ys)