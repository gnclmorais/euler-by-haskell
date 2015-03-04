-- Problem 9 — Special Pythagorean triplet
-- https://projecteuler.net/problem=9

isSquare :: Integer -> Bool
isSquare x = let x' = truncate $ sqrt (fromIntegral x::Double) in x' * x' == x

-- a + b + c -> a -> b -> [a, b, c]
generateTriplet :: Integer -> Integer -> Integer -> [Integer]
generateTriplet s a b
    | isSquare c' &&
      a + b + c == s = [a, b, c]
    | b < div s 2    = generateTriplet s a (b + 1)
    | a < div s 3    = generateTriplet s (a + 1) (a + 2)
    | otherwise      = [-1]
    where c' = a^2 + b^2
          c  = floor (sqrt (fromIntegral c'::Double))

-- The sum of a + b + c -> [a, b, c]
pythagoreanTriplet :: Integer -> [Integer]
pythagoreanTriplet max = generateTriplet max 1 2

-- Main section
main = do
    let triplet = pythagoreanTriplet 1000
    print triplet
    print (sum triplet)
    print (product triplet)
