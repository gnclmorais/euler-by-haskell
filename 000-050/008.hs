-- Problem 8 — Largest product in a series
-- https://projecteuler.net/problem=8

import Data.Char

-- Simple Char to String ('4' -> 4)
toInt :: Char -> Integer
toInt x = fromIntegral (digitToInt x)

-- Slices a string, returning a substring
slice :: String -> Integer -> Integer -> String
slice xs from to =
    take (fromInteger (to - from + 1)) (drop (fromInteger from) xs)

-- Number as string -> Product of all its digits
stringToProduct :: String -> Integer
stringToProduct []     = 0
stringToProduct [x]    = toInt x
stringToProduct (x:xs) = (toInt x) * (stringToProduct xs)

-- String -> Substring length (n) -> List of possible substring with n length
differentNStrings :: String -> Integer -> [String]
differentNStrings [] _ = []
differentNStrings (x:xs) n
    | fromIntegral (length xs) < n = [(x:xs)]
    | otherwise     = [slice (x:xs) 0 (n - 1)] ++ differentNStrings xs n

-- Huge number as string -> n adjacent numbers ->  Greatest product
greatestProductAdjDigits :: String -> Integer -> Integer
greatestProductAdjDigits s n =
    let strings = differentNStrings s n
    in maximum (map (\x -> stringToProduct x) strings)


-- Main section
main = do
    let number = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    --print (stringToProduct "1234")
    --print (greatestProductAdjDigits number)

    --print (differentNStrings "21345" 3)
    --print (greatestProductAdjDigits "21345" 3)

    --print (differentNStrings number 13)
    print (greatestProductAdjDigits number 13)
