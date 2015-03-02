-- Problem 3 — Largest prime factor
-- https://projecteuler.net/problem=3

-- As taken from http://stackoverflow.com/a/4695002/590525
isPrime :: Integer -> Bool
isPrime k = null [x | x <- [2..k - 1], k `mod`x  == 0]

-- Number -> Factor -> Prime factors
primeFactors :: Integer -> Integer -> [Integer]
primeFactors x y
    | isPrime x      = [x]
    | x `mod` y == 0 = (primeFactors (div x y) 2) ++ [y]
    | otherwise      = (primeFactors x (y + 1)) ++ []

-- Number -> Prime factor
largestPrimeFactor :: Integer -> Integer
largestPrimeFactor x =
    maximum (primeFactors x 2)

-- Main section
main = do
    print (largestPrimeFactor 600851475143)
