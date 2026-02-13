-- Worked Example 1:

heartMonitor :: Int -> Int -> String
heartMonitor age bpm
  | age > 80  && bpm > 100 = "High heart rate for 81+!"
  | age > 60  && bpm > 130 = "High heart rate for 61-80!"
  | age > 40  && bpm > 140 = "High heart rate for 41-60!"
  | age > 20  && bpm > 155 = "High heart rate for 21-40!"
  | age >= 0  && bpm > 170 = "High heart rate for 0-20!"
  | otherwise              = "Normal heart rate"


-- Worked Example 2:

pizzaCalories :: Int -> String -> Float
pizzaCalories diameter toppings = (11.5 + toppingCalories) * area
  where
    area = pi * (fromIntegral diameter / 2) ^ 2

    toppingCalories
      | toppings == "pepperoni" = 6
      | toppings == "tuna"      = 4
      | toppings == "veggie"    = 2.5
      | otherwise               = 0

--1

absolute :: Int -> Int
absolute x
  | x < 0 = -x
  | otherwise = x

--2

sign :: Int -> Int
sign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

--3

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == y && y == z = 3
  | x == y = 2
  | x == z = 2
  | y == z = 2
  |otherwise = 0


--4

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths s1 s2 s3 = diagonal s1 + diagonal s2 + diagonal s3
  where
    diagonal s = sqrt (2 * s ^ 2)

--5

taxiFare :: Int -> Float
taxiFare distance
  | distance <= 10 = basefare + fromIntegral distance * 0.5
  | otherwise      = basefare + 10 * 0.5 + fromIntegral (distance - 10) * 0.3
  where
    basefare = 2.20

--6

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
  | x > avg && y > avg && z > avg = 3
  | x > avg && y > avg            = 2
  | x > avg && z > avg            = 2
  | y > avg && z > avg            = 2
  | x > avg                       = 1
  | y > avg                       = 1
  | z > avg                       = 1
  | otherwise                     = 0
  where
    avg = (x + y + z) `div` 3

--7

validDate :: Int -> Int -> Bool
validDate day month
  | month < 1 || month > 12 = False
  | month == 2              = day >= 1 && day <= 28
  | month `elem` [4,6,9,11] = day >= 1 && day <= 30
  | otherwise               = day >= 1 && day <= 31

--8

daysInMonth :: Int -> Int -> Int
daysInMonth month year
  | month == 2 && isLeapYear = 29
  | month == 2               = 28
  | month `elem` [4,6,9,11]  = 30
  | otherwise                = 31
  where
    isLeapYear = year `mod` 4 == 0

{- Written Exercises:

-- Evaluate sumThree Expressions

(a) sumThree 3 5 7
  = 3 + 5 + 7
  = 8 + 7
  = 15

(b) sumThree 8 (1 + 3) 2
  = 8 + (1 + 3) + 2
  = 8 + 4 + 2
  = 12 + 2
  = 14

--Evaluate threeDifferent Expressions

(a) threeDifferent 1 4 2
  = (1 /= 4) && (4 /= 2) && (1 /= 2)
  = True && (4 /= 2) && (1 /= 2)
  = True && True && (1 /= 2)
  = True && True && True
  = True && True
  = True

(b) threeDifferent 1 7 7
  = (1 /= 7) && (7 /= 7) && (1 /= 7)
  = True && (7 /= 7) && (1 /= 7)
  = True && False && (1 /= 7)
  = False && (1 /= 7)
  = False

-- Evaluate howManyEqual Expressions

(a) howManyEqual 3 5 2
  Substitute x=3, y=5, z=2, then check guards:
  
  Guard 1: x == y && y == z
         = 3 == 5 && 5 == 2
         = False && 5 == 2
         = False
  
  Guard 2: x == y         = 3 == 5
         = False
  
  Guard 3: x == z
         = 3 == 2
         = False
  
  Guard 4: y == z
         = 5 == 2
         = False
  
  Guard 5: otherwise
         = True   
  
  Result = 0

(b) howManyEqual 5 2 5
  Substitute x=5, y=2, z=5, then check guards:
  
  Guard 1: x == y && y == z
         = 5 == 2 && 2 == 5
         = False && 2 == 5
         = False
  
  Guard 2: x == y
         = 5 == 2
         = False
  
  Guard 3: x == z
         = 5 == 5
         = True
-}

