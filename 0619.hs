--Square subsets
--Problem 619
--For a set of positive integers {a,a+1,a+2,…,b} , let C(a,b) be the number of non-empty subsets in which the product of all elements is a perfect square.
--For example C(5,10)=3, since the products of all elements of {5,8,10}, {5,8,9,10} and {9} are perfect squares, and no other subsets of {5,6,7,8,9,10} have this property.
--You are given that C(40,55)=15, and C(1000,1234) mod 1000000007=975523611.
--Find C(1000000,1234567) mod 1000000007.

import qualified Data.Numbers.Primes as NP
import           EulerFunctions

-- The set of nonempty subsets of [a..b] whose products are perfect squares
c a b = filter (\xs -> isSquare $ product xs ) (powerset set)
    where set = filter (not . NP.isPrime) [a..b]


isSquare n = x * x == n
    where x = truncate $ sqrt $ fromIntegral n

-- GENERAL STRATEGY
-- You can filter out numbers which are prime, because obviously no product of a subset of [a..b] containing a prime will be square, as a square contains an even number of each of its prime factors, and primes only have one prime factor.
