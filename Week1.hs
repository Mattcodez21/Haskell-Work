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
volumeOfCylinder h r = areaOfCircle r * h

--5

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((y1 - y2)^2 + (x1 - x2)^2) 

--6

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = (x /= y) && (y /= z) && (x /= z)

--7

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = mod x y == 0

--8

isEven :: Int -> Bool
isEven x = divisibleBy x 2

--9

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3