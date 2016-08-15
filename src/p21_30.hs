import Data.List

-- Problem 21
-- Insert an element at a given position into a list.

insert' :: Int -> a -> [a] -> [a]
insert' n x xs = (take n xs) ++ (x : drop n xs)

-- Problem 22
-- Create a list containing all integers within a given range.

range' :: Int -> Int -> [Int]
range' lower upper = [lower..upper]

-- Problem 28
-- Sorting a list of lists according to length of sublists

compareListLengths :: [a] -> [a] -> Ordering
compareListLengths xs ys
    | length xs < length ys = LT
    | length xs > length ys = GT
    | otherwise = EQ

lengthSort' :: [[a]] -> [[a]]
lengthSort' = sortBy compareListLengths
