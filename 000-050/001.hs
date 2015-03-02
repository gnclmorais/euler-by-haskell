-- Problem 1 — Multiples of 3 and 5
-- https://projecteuler.net/problem=1

-- Index -> Acumulator -> Result
multiples3and5 :: Integer -> Integer -> Integer
multiples3and5 0 y = y
multiples3and5 x y
    | x `mod` 3 == 0 = multiples3and5 (x - 1) (y + x)
    | x `mod` 5 == 0 = multiples3and5 (x - 1) (y + x)
    | otherwise      = multiples3and5 (x - 1) y

-- Main section
main = do
    print (multiples3and5 999 0)
