-- Find the maximum total from top to bottom of the triangle below:
-- Note: this is a very inefficient solution
import           EulerFunctions

main = print $ maximum $ map sum (pathList triangle2)

triangle1 = [
    [75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20, 04, 82, 47, 65],
    [19, 01, 23, 75, 03, 34],
    [88, 02, 77, 73, 07, 63, 67]]

triangle2 = [
    [75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20, 04, 82, 47, 65],
    [19, 01, 23, 75, 03, 34],
    [88, 02, 77, 73, 07, 63, 67],
    [99, 65, 04, 28, 06, 16, 70, 92],
    [41, 41, 26, 56, 83, 40, 80, 70, 33],
    [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

-- Return a list of all possible n digit combinations of 0s and 1s using the binary function above, where each element is a list of digits representing that binary number.
almostAllBinaries :: Int -> [[Int]]
almostAllBinaries n = map (addZeroes . binary) [0.. 2^(n-1) - 1] -- I specifically used 2^(n-1) because we want the first digit to always be zero (since on the first row of the triangle there is only one position, which is the zeroth position).
    where
        addZeroes b -- keeps adding zeroes to the argument b until the number of elements in b is equal to n.
            | length b < n  = addZeroes (0 : b)
            | length b == n = b

-- positions in the triangle. Takes a list of 1s and 0s and returns a list of x coordinates (on a horizontal axis) that represent which column you are on in any given row
positions :: [Int] -> [Int]
positions binlist = map (\x -> sum $ take x binlist ) [1 .. length binlist]
-- This will give positions on each column, starting with index 0.

-- Returns the path through the triangle as determined by the position list given by positions.
-- path :: [[Int]] -> [Int] -> [Int]
path triangle poslist = zipWith (\x l -> l !! x) poslist triangle

pathList triangle = map (path triangle) (map positions (almostAllBinaries $ length triangle)) -- Gives you list of all possible positions
