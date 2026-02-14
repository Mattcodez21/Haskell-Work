-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||))

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True = True
False || True = True
True || False = True
False || False = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a

fact :: Int -> Int
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)
  | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
  | n == 0 = 0
  | n > 0 = m + mult (n - 1) m
  | otherwise = - mult (- n) m

divide :: Int -> Int -> Int
divide n m
  | n < m = 0
  | otherwise = 1 + divide (n - m) m 


--Worked Example 1:

nor :: Bool -> Bool -> Bool
nor False x = not x
nor True _  = False

--Worked Example 2:

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


--1

(&&) :: Bool -> Bool -> Bool
False && False = False
False && True  = False
True  && False = False
True  && True  = True

--2

exOr :: Bool -> Bool -> Bool
exOr x y = x /= y 

--3

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True  x _ = x
ifThenElse False _ y = y

--4

daysInMonth :: Int -> Int
daysInMonth 2  = 28
daysInMonth 4  = 30
daysInMonth 6  = 30
daysInMonth 9  = 30
daysInMonth 11 = 30
daysInMonth _  = 31

--5

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

--6

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n^2 + sumSquares (n - 1)

--7

power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n - 1)

--8

sumFromTo :: Int -> Int -> Int
sumFromTo x y
  | y < x     = 0
  | x == y    = x
  | otherwise = x + sumFromTo (x + 1) y

--9

gcd :: Int -> Int -> Int
gcd x y
  | x == y    = x
  | x > y     = Prelude.gcd (x - y) y
  | otherwise = Prelude.gcd x (y - x)

