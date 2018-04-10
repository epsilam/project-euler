main = print $ primeSum 2000000

primeSum n = sum (primes n) where
    primes n = 2 : 3 : sieve (filter (\x -> not (mod x 3 == 0)) [5,7 .. n]) where
        sieve (p:xs)
            | length (p:xs) > 1 = p : sieve [x | x <- xs, mod x p /= 0]
            | length (p:xs) == 1 = [p]
