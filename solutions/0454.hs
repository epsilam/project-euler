-- Diophantine reciprocals III
-- Problem 454

-- In the following equation x, y, and n are positive integers.
-- 1/x + 1/y = 1/n
-- For a limit L we define F(L) as the number of solutions which satisfy x < y ≤ L.
-- We can verify that F(15) = 4 and F(1000) = 1069.
-- Find F(10^12).

import           EulerFunctions

-- Implement a naive F(L) for small values of L
f1 l = [(x,y) | y <- [1..l], x <- [1..y - 1],  mod (x*y) (x+y) == 0]

f2 l = [(a*(a+b),b*(a+b)) | b <- [1..l], a <- [1..b-1], b*(a+b) <= l, gcd a b == 1]
