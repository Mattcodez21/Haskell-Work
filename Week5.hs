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
