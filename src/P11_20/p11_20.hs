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
