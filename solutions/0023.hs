-- Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
-- Note: all the integers which cannot be written as the sum of two abundant numbers are less than 28124.
import           Data.List      (group, sort)
import           EulerFunctions

main = print ((sum oddsNonSums) + (sum evensNonSums))

isAbundant n = n < (sum . init . divisors) n
abundantNumbers = filter isAbundant [1..28123]

-- Strategy:
-- Note that very few abundant numbers are odd. This means that most of the numbers which cannot be written as the sum of two abundant numbers will be odd.
-- In principle, if an odd number can be written as the sum of two abundant numbers, then it must be the sum of an even abundant number and an odd abundant number.
-- We can calculate the list of all odd numbers which can be written as the sum of two abundant numbers using this fact by just finding every possible additive combination of odd and even abundant numbers, i.e., [x+y | x <- (filter odd abundantNumbers), y <- abundantNumbers]

-- Below is the list of all odd numbers <= 28123 which can be written as the sum of two abundant numbers.
oddsSumsAbundants = sortRemDups [x+y | x <- (filter odd abundantNumbers), y <- (filter even abundantNumbers), x+y <= 28123]

-- The list of odd numbers <= 28123 which CANNOT be written as the sum of two abundant numbers.
oddsNonSums = filter (\x -> not $ elem x oddsSumsAbundants) [1,3..28123]

-- The list of even numbers <= 28123 which can be written as the sum of two abundant numbers ()
evensSumsAbundants = sortRemDups [x+y | x <- (filter even abundantNumbers), y <- (filter even abundantNumbers), x+y <= 28123]

-- The list of even numbers <= 28123 which CANNOT be written as the sum of two abundant numbers.
evensNonSums = filter (\x -> not $ elem x evensSumsAbundants) [2,4..28123]
