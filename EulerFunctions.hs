module EulerFunctions
    (sortRemDups, applyTimes, isInt, binary, allBinaries, concatDigits, primes, primeFactors, divisors, digits, powerset, modExp, factorial)
    where

import           Control.Monad
import           Data.Bits
import           Data.List     (group, sort)
--import qualified Data.Set      as DS (fromList, toList)

-- Sorts a list and removes duplicates in O(n * log(n))
sortRemDups :: Ord a => [a] -> [a]
sortRemDups = map head . group . sort

-- Returns the nth repeated application of a function to some initial input.
applyTimes :: Integral n => (a -> a) -> a -> n -> a
applyTimes func input n
    | n == 0 = input
    | otherwise = applyTimes func (func input) (n-1)

-- Returns True if n is an integer, False otherwise. Values such as 1 and 1.0 are both integers and thus cause isInt to return True.
isInt :: RealFrac a => a -> Bool
isInt n = n == fromInteger (truncate n)

-- Takes an int and returns a list of all its prime factors.
primeFactors :: Integral a => a -> [a]
primeFactors n
    | n > 1 = leastDivisor : primeFactors (div n leastDivisor)
    | otherwise = []
    where
        leastDivisor = head [i | i <- [2 .. n], mod n i == 0]

-- Returns all numbers which evenly divide n (not just prime factors). divisors n maps the product function to the powerset of the list of prime factors of n and then removes duplicates and sorts. This is much faster than normal trial division.
divisors :: Integral a => a -> [a]
divisors n = sortRemDups $ map product (powerset $ primeFactors n)

-- Here is a naive implementation of a divisor function which uses trial division to check whether each number less than n divides n, and returns those that do.
divisorsNaive n = [x | x <- [1..n], mod n x == 0]

-- Takes an int and returns a list of the digits in its binary representation. E.g., binary 13 -> [1,1,0,1]
binary :: Integral a => a -> [a]
binary n
    | n > 0     = binary (div n 2) ++ [r]
    | otherwise = []
    where r = rem n 2

-- Return a list of all possible n digit combinations of 0s and 1s using the binary function above, where each element is a list of digits representing that binary number.
allBinaries :: Int -> [[Int]]
allBinaries n = map (addZeroes . binary) [0.. 2^n - 1]
    where
        addZeroes b -- keeps adding zeroes to the argument b until the number of elements in b is equal to n.
            | length b < n  = addZeroes (0 : b)
            | length b == n = b

-- Takes a list of digits and concatenates them to produce one number.
concatDigits :: Integral a => [a] -> a
concatDigits xs = foldl (\num x -> 10*num + x) 0 xs

-- List of primes less than or equal to n. This function skips checking even numbers by only sieving the set of odd numbers between 3 and n, and prepending the number 2 to the list.
primes :: Integral a => a -> [a]
primes n = 2 : sieve [3,5 .. n]
    where
        sieve (p:xs)
            | null xs   = [p]
            | otherwise = p : (sieve [x | x <- xs, mod x p /= 0])

digits :: Integral a => a -> [a]
digits n
    | n == 0    = []
    | otherwise = digits (div n 10) ++ [mod n 10]

-- Returns the powerset of a list
-- Credit to Miran Lipovaca (Learn You a Haskell for Great Good)
powerset :: [a] -> [[a]]
powerset xs = filterM (const [True, False]) xs

-- Takes base b, exponent e, and modulus m, and returns b^e (mod m) efficiently.
-- Credit to https://gist.github.com/trevordixon/6788535
modExp :: (Integral a, Bits a) => a -> a -> a -> a
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
    where t = if testBit e 0 then b `mod` m else 1

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * (factorial (n - 1))
