-- Worked example 1

square :: Int -> Int
square n = n * n

circumferenceOfCircle :: Float -> Float
circumferenceOfCircle d = pi * d

sideOfCylinder :: Float -> Float -> Float
sideOfCylinder d h = h * circumferenceOfCircle d

-- Worked example 2

canDrink :: Int -> Bool
canDrink age = age >= 18

all3CanDrink :: Int -> Int -> Int -> Bool
all3CanDrink a b c = canDrink a && canDrink b && canDrink c

-- Questions

-- 1

timesTen :: Int -> Int
timesTen n = n * 10

-- 2

sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

-- 3

areaOfCircle :: Float -> Float
areaOfCircle r = pi * r^2

-- 4

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder r h = areaOfCircle r * h