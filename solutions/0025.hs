main = print $ length $ fibList [1,1]
    where
        fibList (x:y:xs)
            | (length $ show x) == 1000 = x:y:xs
            | otherwise = fibList ((x+y):x:y:xs)
