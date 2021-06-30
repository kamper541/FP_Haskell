e1 = 1
e2 = 2

f a b = a / b

g = let x = 1
        y = 2
    in f x y

h = let y = 2
    in let x = 1
        in f x y

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

zip' x y = zipWith' (,) x y

f' x y = (x < y)

dropFirst :: [a] -> Maybe [a]
dropFirst [] = Nothing
dropFirst (x:xs) = Just xs

addNum x n = addNum' x 13 0
    where
        addNum' (x:xs) n ans
            | n == 1 = ans
            | otherwise = addNum' xs (n-1) (ans + x*n)

finalcal x n = 11 - (addNum x n) `mod` 11
    
--pf = \x n -> 11 - (addNum x n) `mod` 11
--pf' = \x n-> 11 - (`mod` 11) . (addNum x) n

listAns (x:xs) = doIt (x:xs) xs
    where 
        doIt a b = zipWith (<) a b

increasingPairs :: (Ord a, Num a) => [a] -> Int
increasingPairs x = 
    if x == []
        then  0
        else length (filter (== True) (listAns x))

zipWith'' x y = let z = zip x y
                in ()