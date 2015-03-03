-- Problem 6 — Sum square difference
-- https://projecteuler.net/problem=6

-- n -> Sum of squares of the first nth numbers
sumOfSquares :: Integer -> Integer
sumOfSquares 1 = 1
sumOfSquares n = (n ^ 2) + sumOfSquares (n - 1)

-- n -> Sum of the first nth numbers
sumNatural :: Integer -> Integer
sumNatural 1 = 1
sumNatural n = n + sumNatural (n - 1)
-- n -> Square of the sum of the first nth numbers
squaresOfSum :: Integer -> Integer
squaresOfSum n = (sumNatural n) ^ 2

-- n -> (1 + ... + n)^2 - (1^2 + ... + n^2)
sumSquareDifference :: Integer -> Integer
sumSquareDifference n = (squaresOfSum n) - (sumOfSquares n)

-- Main section
main = do
    print (sumOfSquares 100)
    print (squaresOfSum 100)
    print (sumSquareDifference 100)
