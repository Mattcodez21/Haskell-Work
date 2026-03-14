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
