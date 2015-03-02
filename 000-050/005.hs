-- Problem 5 — Largest palindrome product
-- https://projecteuler.net/problem=5

-- Starter -> Current divisor -> Current dividend -> Result
divisible :: Integer -> Integer -> Integer -> Integer
divisible _ 0 y = y
divisible s x y
    | mod y x == 0 = divisible s (x - 1) y
    | otherwise    = divisible s s (y + 1)

-- Divisable up to -> Smallest number
evenlyDivisible :: Integer -> Integer
evenlyDivisible x = divisible x x x

-- Main section
main = do
    print (evenlyDivisible  20)
