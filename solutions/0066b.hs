-- Find the value of D ≤ 1000 in minimal solutions of x in x^2 - Dy^2 = 1 for which the largest value of x is obtained.
import           EulerFunctions

-- isSquare :: Floating a => a -> Bool
isSquare = isInt . sqrt

-- Given some d, finds the minimal solution of x
xMinimalSoln :: Integral a => a -> a
xMinimalSoln d =
    convGen d [sqrt $ fromIntegral d] [floor $ sqrt $ fromIntegral d] [1]

-- Given d, recursively generates parts of convergents in the continued fraction expansion of sqrt d.
-- Stops and returns p when an integer p has been generated such that (p,q) is a solution to the diophantine for some integer q.
-- convGen :: (Integral a, RealFloat b) => a -> [b] -> [a] -> a
convGen :: (Integral a, RealFrac b) => a -> [b] -> [a] -> [a] -> a
convGen d (r:rs) (p:ps) (q:qs)
    | p^2 - (d * q^2) == 1 = p
    | otherwise =
        convGen d (r':r:rs) (p':p:ps) (q':q:qs)
    where
        r' = 1/(r - (fromIntegral $ floor r))
        p' = (floor r') * p + (f ps)
        q' = (floor r') * q + (f qs)
        f xs
            | length xs == 0 = 1
            | otherwise      = head xs
