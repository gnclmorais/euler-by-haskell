-- Problem 4 — Largest palindrome product
-- https://projecteuler.net/problem=4

-- Number -> Is palindrome
isPalindrome :: Integer -> Bool
isPalindrome x =
    let y = show x
    in y == reverse y

-- [2, 3, 7] -> "237"
joinNrs :: Show a => [a] -> String
joinNrs []     = ""
joinNrs (x:xs) = show x ++ joinNrs xs

-- Number of digits -> Number
smallestNrByDigits :: Integer -> Integer
smallestNrByDigits n =
    let nines = replicate (fromIntegral (n - 1)) 9
    in read (joinNrs nines) + 1

-- Number of digits -> Number
biggestNrByDigits :: Integer -> Integer
biggestNrByDigits n =
    let nines = replicate (fromIntegral n) 9
    in read (joinNrs nines)

-- 1. Multiplay all by all
-- 2. Filter results by isPalindrome
-- 3. Find the longest one
-- Nr of digits -> Result
largestPalindrome :: Integer -> Integer
largestPalindrome x =
    let a = smallestNrByDigits x
        b = biggestNrByDigits x
        r = [a..b]
        l = [x * y | x <- r, y <- r]
    in maximum (filter isPalindrome l)

-- Main section
main = do
    print (largestPalindrome 3)
