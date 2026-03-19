<<<<<<< HEAD
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
=======
--1

headPlusOne :: [Int] -> Int
headPlusOne []     = -1        -- Empty list → return -1
headPlusOne (x:_)  = x + 1     -- Non-empty → take head, add 1

--2

duplicateHead :: [a] -> [a]
duplicateHead []     = []      -- Empty list stays empty
duplicateHead (x:xs) = x : x : xs  -- Add extra copy of head

--3

rotate :: [a] -> [a]
rotate []           = []           -- Empty list
rotate [x]          = [x]          -- One element, can't swap
rotate (x:y:rest)   = y : x : rest -- Swap x and y

--4

listLength :: [a] -> Int
listLength []     = 0                  -- Base case: empty list has length 0
listLength (_:xs) = 1 + listLength xs  -- Recursive: 1 + length of rest

--5

multAll :: [Int] -> Int
multAll []     = 1              -- Base case: product of nothing is 1
multAll (x:xs) = x * multAll xs -- Multiply head by product of rest

--6

andAll :: [Bool] -> Bool
andAll []     = True             -- Base case: all of nothing is True
andAll (x:xs) = x && andAll xs   -- x AND (rest are all true)

--7

orAll :: [Bool] -> Bool
orAll []     = False            -- Base case: any of nothing is False
orAll (x:xs) = x || orAll xs    -- x OR (any of rest are true)

--8

countIntegers :: Int -> [Int] -> Int
countIntegers _ []     = 0  -- Base case: target not in empty list
countIntegers target (x:xs)
  | x == target = 1 + countIntegers target xs  -- Found it! Count and continue
  | otherwise   = countIntegers target xs       -- Not found, continue

--9

removeAll :: Int -> [Int] -> [Int]
removeAll _ []     = []  -- Base case: nothing to remove from empty list
removeAll target (x:xs)
  | x == target = removeAll target xs      -- Found it! Skip it, recurse
  | otherwise   = x : removeAll target xs  -- Keep it, recurse

--10

removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ []     = []
removeAllButFirst target (x:xs)
  | x == target = x : removeAll target xs  -- Found first! Keep it, remove rest
  | otherwise   = x : removeAllButFirst target xs  -- Not found yet, keep looking

--11

listMarks :: String -> [StudentMark] -> [Int]
listMarks _ []     = []  -- Base case: no marks in empty list
listMarks name ((studentName, mark):rest)
  | name == studentName = mark : listMarks name rest  -- Match! Include mark
  | otherwise           = listMarks name rest          -- No match, skip

--12

sorted :: [Int] -> Bool
sorted []       = True  -- Empty list is sorted
sorted [_]      = True  -- Single element is sorted
sorted (x:y:rest)
  | x <= y    = sorted (y:rest)  -- First two in order, check rest
  | otherwise = False             -- Out of order! Not sorted

--13

prefix :: [Int] -> [Int] -> Bool
prefix [] _          = True   -- Empty list is prefix of anything
prefix _ []          = False  -- Non-empty can't be prefix of empty
prefix (x:xs) (y:ys)
  | x == y    = prefix xs ys  -- Heads match, check tails
  | otherwise = False          -- Heads don't match, not a prefix

--14

subSequence :: [Int] -> [Int] -> Bool
subSequence [] _  = True  -- Empty is subsequence of anything
subSequence _ []  = False -- Non-empty can't be in empty list
subSequence needle haystack@(y:ys)
  | prefix needle haystack = True           -- Found it at start!
  | otherwise              = subSequence needle ys  -- Check rest
>>>>>>> 4ec37683989aa551fa205e6b00830bf8f1adf018
