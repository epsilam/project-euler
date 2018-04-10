main = print ans

-- Take first element of the set of triangular numbers which have more than 500 divisors
ans = head $ filter (\x -> (length $ factors x) > 500) $ map tri [1..]
-- Returns nth triangular number
tri :: Integral a => a -> a
tri n = div (n * (n + 1)) 2

-- Takes floor of sqrt of argument
flSq :: Integer -> Integer
flSq = floor . sqrt . fromIntegral

-- Factors of n between 1 and sqrt n inclusive.
factorsLEQSqrt :: Integer -> [Integer]
factorsLEQSqrt n = 1 : [i | i <- [2 .. flSq n], n `mod` i == 0]

-- All factors of n
factors :: Integer -> [Integer]
factors n = let smallFactors = factorsLEQSqrt n in smallFactors ++ map (\x -> n `div` x) (reverse smallFactors)
