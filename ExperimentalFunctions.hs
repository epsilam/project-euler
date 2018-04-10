module ExperimentalFunctions
    (riffleShuffle, rotations)
    where
-- This is a module for functions which I found fun to implement but weren't useful in the final solutions to the Euler problems.

-- Takes a list and rifle-shuffles it. This function was originally made for problem 622 but was far too inefficient to be of any use.
riffleShuffle :: [a] -> [a]
riffleShuffle xs =
    interleave (take half xs) (drop half xs)
    where
        half                     = div (length xs) 2
        interleave (y:ys) (z:zs) = y : z : (interleave ys zs)
        interleave []     []     = []
        interleave []     [z]    = z : (interleave [] [])

-- Takes an int and returns all the possible "rotations" of that int.
rotations :: Integral a => a -> [a]
rotations n = [read ((drop i (show n)) ++ (take i (show n))) :: Int | i <- reverse [1 .. (length (show n))]]
