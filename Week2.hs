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