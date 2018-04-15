-- We approach this problem by thinking of the square of numbers as a set of "layers", where 1 is the center, and the layer containing numbers 2 to 9 is the first layer, the layer containing numbers 10 to 25 is the second layer, etc.
-- We can find an expression for the sum of the corners in any given layer by finding expressions for each number in each corner of the nth layer.
-- Notice that in layer n, the number in the top right corner is (2n+1)^2, the number in the bottom left is (2n)^2 + 1, and hence the numbers in the top left and bottom right are (2n)^2 + 1 + 2n and (2n)^2 + 1 - 2n respectively.
-- Thus, the sum of the corners at the nth layer is
-- (2n+1)^2 + (2n)^2 + 1 + (2n)^2 + 1 + 2n + (2n)^2 + 1 - 2n
-- = 4(4n^2 + n + 1).
-- Since a 1001 by 1001 square has 500 layers (by our definition of a layer above), we simply take the sum of this expression over n <- [1..500].
-- Of course, don't forget to add the 1 from the center.

main = print $ (1 + sum [4*(4*n^2+n+1)| n <- [1..500]])
