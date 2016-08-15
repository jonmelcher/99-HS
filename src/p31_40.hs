-- Problem 31
-- (**) Determine whether a given integer number is prime.
intSqrt :: Int -> Int
intSqrt = floor . (sqrt . fromIntegral)

isPrime' :: Int -> Bool
isPrime' n
    | n < 2 = False
    | otherwise = all (\i -> (mod n i /= 0)) [2..intSqrt n]

-- Problem 32
-- (**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (mod a b)

-- Problem 33
-- (*) Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.
areCoprime' :: Int -> Int -> Bool
areCoprime' a b = (gcd' a b) == 1

-- Problem 34
-- (**) Calculate Euler's totient function phi(m).
totient' :: Int -> Int
totient' m = length (filter (\i -> areCoprime' m i) [1..m])

-- Problem 39
-- (*) A list of prime numbers.
primes' :: Int -> Int -> [Int]
primes' lower upper = filter isPrime' [lower..upper]

-- Problem 36
-- (**) Determine the prime factors of a given positive integer.
primeFactors' :: Int -> [Int]
primeFactors' n = filter (\i -> isPrime' i && mod n i == 0) (n : [1..quot n 2])


-- Problem 40
-- (**) Goldbach's conjecture.
-- Write a predicate to find the two prime numbers that sum up to a given even integer.
goldbach' :: Int -> (Int, Int)
goldbach' n
    | mod n 2 /= 0 = error("n must be even")
    | otherwise = head [(i, j) | i <- filter isPrime' [1..n], j <- filter isPrime' [1..n], i + j == n]
