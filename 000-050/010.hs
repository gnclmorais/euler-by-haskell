-- Problem 10 — Summation of primes
-- https://projecteuler.net/problem=10

-- Apply a function to a list and returns true if at least one element matches
some :: (a -> Bool) -> [a] -> Bool
some _ []     = False
some f (x:xs) = if (f x) then True else (some f xs)

-- Range 0..n -> Dividors 0..√n -> Result
removeNonPrimes :: [Integer] -> [Integer] -> [Integer]
removeNonPrimes [] _ = []
removeNonPrimes (x:xs) y
    | some (\z -> mod x z == 0 && div x z > 1) y = removeNonPrimes xs y
    | otherwise                        = [x] ++ removeNonPrimes xs y

-- Sieve of Eratosthenes, as seen on
-- http://www.mathblog.dk/sum-of-all-primes-below-2000000-problem-10/
-- The nth prime number -> Sum of them all
primesSum :: Integer -> Integer
primesSum n =
    sum (removeNonPrimes [2..n] [2..ceiling (sqrt (fromIntegral n))])

-- Main section
main = do
    --print (primesSum 10)
    print (primesSum 2000000)
