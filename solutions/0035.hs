import           Data.Numbers.Primes

main = print $ length [n | n <- ps, circular n]
ps = takeWhile (<1000000) primes
circular n = not $ elem False $ map isPrime (rotations n)
rotations n = [read ((drop i (show n)) ++ (take i (show n))) :: Int | i <- reverse [1 .. (length (show n))]]
