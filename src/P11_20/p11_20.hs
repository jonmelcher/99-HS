-- Problem 14
-- (*) Duplicate the elements of a list.

duplicate' :: [a] -> [a]
duplicate' = foldr (\x acc -> x : x : acc) []

-- Problem 15
-- (**) Replicate the elements of a list a given number of times.

ncompose' :: (Enum a, Num a, Ord a) => a -> (c -> c) -> c -> c
ncompose' n f
    | n < 1 = error("out of range")
    | otherwise = foldl (.) id (map (\x -> f) [1..n])

nplicate' :: Int -> [a] -> [a]
nplicate' n = foldr (\x acc -> (ncompose' n (x:)) acc) []

-- Problem 16
-- (**) Drop every N'th element from a list.

ndrop' :: Int -> [a] -> [a]
ndrop' 0 xs = error("out of range")
ndrop' n xs = map (fst) $ filter ((n /=) . snd) $ zip xs (cycle [1..n])

-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given.

nsplit' :: Int -> [a] -> ([a], [a])
nsplit' n xs = (take n xs, drop n xs)

-- Problem 18
-- (**) Extract a slice from a list.

slice' :: Int -> Int -> [a] -> [a]
slice' i j xs = take (j - i + 1) (drop (i - 1) xs)


-- Problem 19
-- (**) Rotate a list N places to the left.

nrotate' :: Int -> [a] -> [a]
nrotate' n xs = drop (mod n $ length xs) xs ++ take (mod n $ length xs) xs

-- Problem 20
-- (*) Remove the K'th element from a list.

drop' :: Int -> [a] -> (a, [a])
drop' k xs = (xs !! k, take (k - 1) xs ++ drop k xs)
