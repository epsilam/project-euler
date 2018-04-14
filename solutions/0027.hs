import           Data.List           (sort)
import           Data.Numbers.Primes (isPrime)

main  = print $ snd $ last $ sort [(l a b, a*b) | a <- [-999..999], b <- [-1000..1000]]
l a b = length $ takeWhile isPrime (map (\n -> n^2 + a*n + b) [0..])
