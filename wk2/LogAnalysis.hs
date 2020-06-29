--{-# OPTIONSzGHC -Wall #-}
module LogAnalysis where
import Log

splitString :: String -> [String]
splitString x = words x

splitFile :: String -> [[String]]
splitFile x = map words (lines x)


parseMessage :: [String] -> LogMessage
parseMessage xs =
  case xs of
    ("I":z:ys)   -> LogMessage Info (read z::Int) (unwords ys)
    ("W":z:ys)   -> LogMessage Warning (read z::Int) (unwords ys)
    ("E":x:z:ys) -> LogMessage (Error (read x::Int)) (read z::Int) (unwords ys)
    _            -> Unknown (unwords xs)

--helper :: [String] -> LogMessage
--helper = parseMessage . splitString
--
parse :: String -> [LogMessage]
parse x = map parseMessage (splitFile x) --need some kind of map here

main :: IO ()
main = do
    let x = splitString "E 2 562 help help"
    putStrLn (show (parseMessage x))
    let x = splitString "I 29 la la la"
    putStrLn (show (parseMessage x))
    let x = splitString "This is not in the right format"
    putStrLn (show (parseMessage x))
