module Golf where

skips :: [a] -> [[a]]
skips xs = map h [1..length xs] where h = \y -> map fst $ filter (\x -> snd x `mod` y == 0) $ zip xs [1..length xs]
--zip list with its own length, to get a list of tuples with index number (like Python enumerate)
--then first lambda gives the list tuple where index is divisble by y
--second lambda (h) filters the list by divisibility by y and returns list of first element
--finally we map indices of list ([1..length xs]) to second lambda

localMaxima :: [Integer] -> [Integer]
localMaxima (a:xs@(b:c:_)) = [b | b > a && b > c] ++ localMaxima xs
localMaxima _ = []

histogram :: [Integer] -> String
