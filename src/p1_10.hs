-- Problem 1
-- (*) Find the last element of a list.

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

-- Problem 2
-- (*) Find the last but one element of a list.

penultimate' :: [a] -> a
penultimate' [] = error "empty list"
penultimate' [x] = error "one element list"
penultimate' (y:[x]) = y
penultimate' (_:xs) = penultimate' xs

-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.

kthElement' :: [a] -> Int -> a
kthElement' [] _ = error "out of range"
kthElement' (x:_) 1 = x
kthElement' (_:xs) k
    | k < 1 = error "out of range"
    | otherwise = kthElement' xs (k - 1)

-- Problem 4
-- (*) Find the number of elements of a list.

length' :: [a] -> Int
length' = sum . map (\x -> 1)

-- Problem 5
-- (*) Reverse a list.

reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = reverse' (tail xs) ++ [head xs]

-- Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' xs = (head xs == last' xs) && (isPalindrome' $ init $ tail xs)

-- Problem 7
-- (**) Flatten a nested list structure.

data NestedList a = Elem a | List [NestedList a]

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List xs) = concatMap flatten' xs

-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.

eliminateConsecutiveDuplicates' :: (Eq a) => [a] -> [a]
eliminateConsecutiveDuplicates' [] = []
eliminateConsecutiveDuplicates' [x] = [x]
eliminateConsecutiveDuplicates' (x:y:xs)
    | x == y = eliminateConsecutiveDuplicates' (y:xs)
    | otherwise = x : eliminateConsecutiveDuplicates' (y:xs)

-- Problem 9
-- (**) pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.

packConsecutiveDuplicates' :: Eq a => [a] -> [[a]]
packConsecutiveDuplicates' [] = []
packConsecutiveDuplicates' xs = takeWhile (== head xs) xs : packConsecutiveDuplicates' (dropWhile (== head xs) xs)

-- Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement
-- the so-called run-length encoding data compression method. Consecutive duplicates
-- of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

runLengthEncoding' :: Eq a => [a] -> [(Int, a)]
runLengthEncoding' = map (\grp -> (length grp, head grp)) . packConsecutiveDuplicates'
