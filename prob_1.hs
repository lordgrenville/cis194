toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x < 10    = [x]
  | otherwise = [x `mod` 10] ++ toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

doubleEveryOther_ :: [Integer] -> [Integer]
doubleEveryOther_ [] = []
doubleEveryOther_ [x] = [x]
doubleEveryOther_ (x:y:zs) = x:2*y: doubleEveryOther_ zs

doubleEveryOther = reverse . doubleEveryOther_ . reverse

sumLongNumber :: Integer -> Integer
sumLongNumber n
  | (n `div` 10) < 1 = n
  | otherwise = n `mod` 10 + sumLongNumber (n `div` 10)


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sumLongNumber x
sumDigits (x:xs) = sumLongNumber x + sumDigits xs

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

main :: IO ()
main = do
    putStrLn (show (validate (4012888888881881)))
    putStrLn (show (validate (4012888888881882)))
    putStrLn "Please enter number for validation"
    x <- getLine
    let y = (read x :: Integer)
    let res = show (validate y)
    putStrLn (res)
