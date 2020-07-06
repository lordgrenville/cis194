{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

splitFile :: String -> [[String]]
splitFile x = map words (lines x)

parseMessage :: [String] -> LogMessage
parseMessage xs =
  case xs of
    ("I":z:ys)   -> LogMessage Info (read z::Int) (unwords ys)
    ("W":z:ys)   -> LogMessage Warning (read z::Int) (unwords ys)
    ("E":x:z:ys) -> LogMessage (Error (read x::Int)) (read z::Int) (unwords ys)
    _            -> Unknown (unwords xs)

parse :: String -> [LogMessage]
parse x = map parseMessage (splitFile x)

--helper func for testing below
splitString :: String -> [String]
splitString x = words x

insert :: LogMessage -> MessageTree -> MessageTree
insert l@(LogMessage _ _ _) Leaf = Node Leaf l Leaf
insert l@(LogMessage _ t1 _) (Node left lm@(LogMessage _ t2 _)  right) = if t1 < t2 then Node (insert l left) lm right  else Node left lm (insert l right)
insert _ tree = tree --Unknown

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder  Leaf = []
inOrder (Node l msg r) = (inOrder l) ++ [msg] ++ (inOrder r)

logFilter :: [LogMessage] -> [LogMessage]
logFilter [] = []
logFilter [lm@(LogMessage (Error x) _ _)] = if x > 50 then [lm] else []
logFilter [_] = []
logFilter (x:xs) = logFilter [x] ++ logFilter xs

getText :: LogMessage -> String
getText (LogMessage _ _ text) = text --warning of incomplete pattern but OK in this case

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = map getText(inOrder (build (logFilter x)))

