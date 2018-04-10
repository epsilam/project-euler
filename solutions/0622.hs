-- Riffle Shuffles
-- Problem 622
-- A riffle shuffle is executed as follows: a deck of cards is split into two equal halves, with the top half taken in the left hand and the bottom half taken in the right hand. Next, the cards are interleaved exactly, with the top card in the right half inserted just after the top card in the left half, the 2nd card in the right half just after the 2nd card in the left half, etc. (Note that this process preserves the location of the top and bottom card of the deck).
-- Let s(n) be the minimum number of consecutive riffle shuffles needed to restore a deck of size n to its original configuration, where n is a positive even number. Amazingly, a standard deck of 52 cards will first return to its original configuration after only 8 perfect shuffles, so s(52)=8. It can be verified that a deck of 86 cards will also return to its original configuration after exactly 8 shuffles, and the sum of all values of n that satisfy s(n)=8 is 412.
-- Find the sum of all values of n that satisfy s(n)=60.

import qualified Data.Set       as DS (fromList, toList)
import           EulerFunctions as EF

main = print $ sum $ deckSizesRequiringXRiffles 60

-- Takes an Int x and returns all the numbers n such that it takes x riffle shuffles to return a deck of n cards to its original state. Only searches through the divisors of 2^x - 1 mapped by (+1).
deckSizesRequiringXRiffles x = [i | i <- set, s i == x] where set = map (+1) (divisors (2^x - 1))

-- s n is the minimal number of consecutive riffle shuffles needed to restore a deck of size n to its original configuration (where n is a positive even number). s n is therefore also equal to the multiplicative order of 2 modulo n-1, i.e., s n returns the minimal k such that 2^k congruent to 1 (mod n-1). See http://mathworld.wolfram.com/RiffleShuffle.html and http://mathworld.wolfram.com/Out-Shuffle.html for more information.
s :: Int -> Int
s 2 = 1
s n = head $ filter (\k -> modExp 2 k (n-1) == 1) [1..]
