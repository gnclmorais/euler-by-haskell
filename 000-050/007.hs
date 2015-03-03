-- Problem 7 — 10001st prime
-- https://projecteuler.net/problem=7

-- Apply a function to a list and returns true if at least one element matches
some :: (a -> Bool) -> [a] -> Bool
some _ []     = False
some f (x:xs) = if (f x) then True else (some f xs)

-- Current index -> Current list of primes -> Result
generatePrimes :: Integer -> [Integer] -> [Integer]
generatePrimes 0 (x:xs) = xs
generatePrimes i []     = generatePrimes i [2]
generatePrimes i (x:xs)
    | some (\y -> mod x y == 0) xs = generatePrimes i (x + 1:xs)
    | otherwise                    = generatePrimes (i - 1) (x + 1:x:xs)

-- The nth index -> The nth prime number
primeByIndex :: Integer -> Integer
primeByIndex 0 = 0
primeByIndex n = head (generatePrimes n [])

-- Main section
main = do
    print (primeByIndex 10001)
