module Golf where
import Data.List

skips :: [a] -> [[a]]
skips xs = map h [1..length xs] where h = \y -> map fst $ filter (\x -> snd x `mod` y == 0) $ zip xs [1..length xs]
--zip list with its own length, to get a list of tuples with index number (like Python enumerate)
--then first lambda gives the list tuple where index is divisble by y
--second lambda (h) filters the list by divisibility by y and returns list of first element
--finally we map indices of list ([1..length xs]) to second lambda

localMaxima :: [Integer] -> [Integer]
localMaxima (a:xs@(b:c:_)) = [b | b > a && b > c] ++ localMaxima xs
localMaxima _ = []

getFreqTuples :: [Integer] -> [(Integer, Integer)]
getFreqTuples = map (\x -> (head x, toInteger $ length x)) . group . sort

getMaxRepeat :: [Integer] -> Integer
getMaxRepeat x = toInteger $ maximum $ map snd $ getFreqTuples x

genRow :: [Bool] -> String
genRow x = unwords $ map (\x -> if x then "*" else " ") x

rowFromList :: [(Integer, Integer)] -> Integer -> [Bool]
--given a set of tuples and row index, return those from orig list with at
--least index-many apearancwes
rowFromList xs n =  map (`elem` li) [0..9] where li = nub $ map fst $ filter (\x -> snd x >= n) xs

getBoolArrays :: [Integer] -> [[Bool]]
getBoolArrays x = map (rowFromList y) $ (\z -> [1..getMaxRepeat z]) x where y = getFreqTuples x

histogram :: [Integer] -> String
histogram x = unlines (reverse (map genRow $ getBoolArrays x)) ++ "===================\n0 1 2 3 4 5 6 7 8 9\n"
