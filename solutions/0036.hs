-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

import           EulerFunctions
-- use digits, binary
main = print $ sum [n | n <- [1..999999], pal n]
-- pal :: Int -> Bool
pal n = d n == reverse (d n) && b n == reverse (b n)
    where
        d n = digits n
        b n = binary n
