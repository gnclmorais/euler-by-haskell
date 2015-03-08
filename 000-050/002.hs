-- Problem 2 — Even Fibonacci numbers
-- https://projecteuler.net/problem=2

-- Max number -> n-1 -> n-2 -> Result
generateFibonacci :: Integer -> Integer -> Integer -> [Integer]
generateFibonacci max 0 _ =
    (generateFibonacci max 1 0) ++ []
generateFibonacci max n1 n2
    | n1 < max  = (generateFibonacci max (n1 + n2) n1) ++ [n2]
    | otherwise = [n2]

-- Max number -> Result
evenFibonacci :: Integer -> Integer
evenFibonacci max =
    let fibonaccis = generateFibonacci max 0 0
    in foldl (\acc x -> if even x then acc + x else acc) 0 fibonaccis

{-
fibs = go 1 1
    where
        go i j = i : go j (i + j)

takeUntil p [] = []
takeUntil p (x:xs) = if not (p x) then x : takeUntil p xs else []

evenFibonacci n = sum $ filter even $ takeUntil (>= n) fibs
-}

-- Main section
main = do
    print (evenFibonacci 4000000)
