import qualified Data.List       (sort)
import qualified Data.List.Split (splitOn)
import qualified Data.Set        (fromList, toList)
import qualified EulerFunctions  as EF

euler0001 =
    sum $ filter (\x -> (mod x 3 == 0) || (mod x 5 == 0)) [1 .. 999]

euler0002 =
    sum $ filter even fibsLessThan
    where
        fibsLessThan = takeWhile (<= 4000000) (map fibonacci [1..])
        fibonacci n  = round ((((1 + sqrt 5)/2)^n - ((1 - sqrt 5)/2)^n)/(sqrt 5)) -- makes use of binet's formula

euler0003 =
    maximum $ primeFactors 600851475143
    where
        primeFactors n
            | n > 1 = leastDivisor : primeFactors (div n leastDivisor)
            | otherwise = []
            where leastDivisor = head [i | i <- [2 .. n], mod n i == 0]

euler0004 =
    head [n | n <- palindromes, elem True (isProductOfTwo3DigNums $ realToFrac n)]
        where
            palindromes              = filter isPalindrome $ reverse [100^2 .. 999^2]
                -- Set of all palindromic numbers between 100^2 and 999^2 in reverse order.
            isPalindrome n           = (read (reverse $ show n) :: Integer) == n
                -- Tests if n is a palindrome.
            splitB4Point x       = Data.List.Split.splitOn "." x !! 0
                -- Takes a string and returns the same string, except only with the characters before the character '.'
            splitAfPoint x       = read (Data.List.Split.splitOn "." x !! 1) :: Int
                -- Same as above, except after '.', and converts this string into an Int.
            isProductOfTwo3DigNums n = map ((\x -> (length (splitB4Point x) == 3) && (splitAfPoint x == 0)) . show . (\x -> n/x)) [100 .. 999]
                -- Takes a number n and checks if dividing it by all possible 3 digit numbers ever results in another 3 digit number (thus checking if n is divisible by two 3-digit numbers).

euler0006 =
    diff 100
    where
        diff n = (div (n * (n + 1)) 2)^2 - (sum $ map (^2) [1..n])

euler0007 =
    (sieve [2..]) !! 10000
    where
        sieve (p:xs) =
            p : sieve [x | x <- xs, mod x p /= 0]

euler0009 =
    head [product [k * (m^2 - n^2), k * 2 * m * n, k * (m^2 + n^2)] | k <- (EF.divisors 500), m <- [1..500], n <- [1..500], m > n, 2 * k * (m^2 + m * n) == 1000]
    -- Generate pythagorean triples using the seed (m,n) in [1..1000]^2
    -- A pythagorean triple is a triple (a,b,c) where a = k(m^2 - n^2), and b = k(2mn), and c = k(m^2 - n^2) for arbitrary integers m>n>0.
    -- Notice that a+b+c = 1000 is equivalent to k(m^2 - n^2) + k(2mn) + k(m^2 + n^2) = k(m^2 - n^2 + 2mn + m^2 - n^2) = 2k(m^2 - mn) = 1000
    -- So we want to find a triple of integers k,m,n such that 2k(m^2 - mn) = 1000
    -- We can specify m in [1..1000] and n in [1..1000], but by the equation above, we know that k is a factor of 500.

euler0010 =
    sum $ takeWhile (< 2000000) (sieve [2..])
    where
        sieve (p:xs) =
            p : sieve [x | x <- xs, mod x p /= 0]

euler0014 =
    -- Maps the length of the collatzSequence onto [1..10^6-1] and zips this with [1..10^6-1]. Then sorts the list of tuples (length $ collatzSequence n, n), resulting in the tuple with the largest length $ collatzSequence n being last, and then returns the corresponding n.
    snd $ last $ Data.List.sort $ zip (map (length . collatzSequence) [1..limit]) [1..limit]
    where
        limit = 10^6 - 1
        collatzSequence n = collatzList [n]
        collatzList (x:xs)
            | x == 1    = (x:xs)
            | otherwise = collatzList ((collatz x) : (x:xs))
        collatz n
            | even n = div n 2
            | odd  n = 3 * n + 1

euler0016 =
    sum $ EF.digits (2^1000)

euler0017 =
    sum $ map (length . wordNum) [1..1000]
    where
        wordNum n
            | n >= 0    && n < 20   = simpleDigits !! n
            | n >= 20   && n < 100  = (tensDigits !! ((EF.digits n !! 0) - 2)) ++ (wordNum (EF.digits n !! 1))
            | n >= 100  && n < 1000 = (wordNum $ EF.digits n !! 0) ++ "hundred" ++ (if (tail $ EF.digits n) == [0,0] then "" else "and") ++ wordNum (10 * (EF.digits n !! 1) + (EF.digits n !! 2))
            | n == 1000             = "onethousand"
        simpleDigits =
            ["","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
        tensDigits =
            ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

euler0029 =
    length $ Data.Set.toList $ Data.Set.fromList $ concat (map (\b -> [a^b | a <- [2 .. 100]]) [2 .. 100])
    -- The function (toList . fromList) takes a list and returns the same list with duplicate elements removed.

euler0030 =
    sum $ filter (\x -> x == (sum $ map (^5) (EF.digits x))) [2..10^6]
