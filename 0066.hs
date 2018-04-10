import           Data.List      (sort)
import           EulerFunctions

-- main = print answer
-- answer :: Integral a => a
-- answer :: [(Integer, Integer)]
-- answer = [(minimalSoln d, d) | d <- [2..1000], not $ isSquare d]

-- minimalSoln :: (RealFrac a) => a -> a
bruteforceMinimalxSoln :: Integer -> Integer
bruteforceMinimalxSoln d -- = truncate $ head $ filter (isInt . soln) [2..]
    -- We take advantage of the fact that if d is even, then x must be odd, so we skip even numbers. However, if d is odd, then  we cannot necessarily say whether x must be even or odd.
    | even $ d = truncate $ head $ filter (isInt . soln) [3,5..]
    | odd  $ d = truncate $ head $ filter (isInt . soln) [2..]
    where
        soln x = sqrt ((x^2 - 1) / (fromInteger d))
-- You're getting some dumb type error
isSquare :: Integral a => a -> Bool
isSquare n = x * x == n
    where x = truncate $ sqrt $ fromIntegral n

-------------------------------------------------------------------------
--Below is the work on the efficient (non-bruteforce) solution
-- Assuming that d has an infinite continued fraction expansion [a_0; a_1, a_2, a_3, ...], then conFracTerm d n gives the term a_n in the continued fraction expansion of d.
conFracTerm d n = floor $ r n
    where
        r x
            | x == 0    = d
            | otherwise = 1/(t - (fromIntegral $ floor t))
            where
                t = r (x - 1)

xMinimalSoln d =
    
    where
        a n = conFracTerm (sqrt d) n