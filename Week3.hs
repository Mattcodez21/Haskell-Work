-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||), (&&), gcd)

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||
infixr 3 &&

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


-- Worked Example 1:

nor :: Bool -> Bool -> Bool
nor False x = not x
nor True _  = False

-- Worked Example 2:

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)


-- Exercise 1: AND operator (three versions required)

(&&) :: Bool -> Bool -> Bool
False && False = False
False && True  = False
True  && False = False
True  && True  = True

-- Alternative version 1 (comment out above, uncomment below to test):
--(&&) :: Bool -> Bool -> Bool
--True && True = True
--_    && _    = False

-- Alternative version 2 (comment out above, uncomment below to test):
--(&&) :: Bool -> Bool -> Bool
--False && _ = False
--True  && x = x


-- Exercise 2: XOR

exOr :: Bool -> Bool -> Bool
exOr False False = False
exOr False True  = True
exOr True  False = True
exOr True  True  = False


-- Exercise 3: If-Then-Else

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True  x _ = x
ifThenElse False _ y = y


-- Exercise 4: Days in Month

daysInMonth :: Int -> Int
daysInMonth 2  = 28
daysInMonth 4  = 30
daysInMonth 6  = 30
daysInMonth 9  = 30
daysInMonth 11 = 30
daysInMonth _  = 31

-- Simplified validDate using daysInMonth
validDate :: Int -> Int -> Bool
validDate day month = day >= 1 && day <= daysInMonth month


-- Exercise 5: Sum Numbers

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)


-- Exercise 6: Sum Squares

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n^2 + sumSquares (n - 1)


-- Exercise 7: Power

power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n - 1)


-- Exercise 8: Sum From-To

sumFromTo :: Int -> Int -> Int
sumFromTo x y
  | y < x     = 0
  | x == y    = x
  | otherwise = x + sumFromTo (x + 1) y


-- Exercise 9: GCD

gcd :: Int -> Int -> Int
gcd x y
  | x == y    = x
  | x > y     = gcd (x - y) y
  | otherwise = gcd x (y - x)


-- Exercise 10: Integer Square Root

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot n s
  | s * s <= n = s
  | otherwise  = findRoot n (s - 1)


-- Exercise 11: Alternative implementations
-- The task: If many answers use guards, write versions with pattern matching (or vice versa)
-- Show THREE alternative implementations

-- ALTERNATIVE 1: sumNumbers
-- Original version (pattern matching) - see Exercise 5 above:
-- sumNumbers 0 = 0
-- sumNumbers n = n + sumNumbers (n - 1)

-- Alternative with guards:
--sumNumbers :: Int -> Int
--sumNumbers n
--  | n == 0    = 0
--  | otherwise = n + sumNumbers (n - 1)


-- ALTERNATIVE 2: power  
-- Original version (pattern matching) - see Exercise 7 above:
-- power _ 0 = 1
-- power x n = x * power x (n - 1)

-- Alternative with guards:
--power :: Int -> Int -> Int
--power x n
--  | n == 0    = 1
--  | otherwise = x * power x (n - 1)


-- ALTERNATIVE 3: sumFromTo
-- Original version (guards) - see Exercise 8 above:
-- sumFromTo x y
--   | y < x     = 0
--   | x == y    = x
--   | otherwise = x + sumFromTo (x + 1) y

-- Alternative with pattern matching (harder for this one):
--sumFromTo :: Int -> Int -> Int
--sumFromTo x y = if y < x then 0 else if x == y then x else x + sumFromTo (x + 1) y
-- Note: Pattern matching doesn't work well here since we need to compare values
-- This shows that guards are the better choice for this problem!