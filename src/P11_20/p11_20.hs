-- Problem 14
-- (*) Duplicate the elements of a list.

duplicate' :: [a] -> [a]
duplicate' = foldr (\x acc -> x : x : acc) []
