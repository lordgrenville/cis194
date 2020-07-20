\title{Experiment in Literate Haskell}
\author{Lord Grenville}

\maketitle

\textbf{Wow some Haskell code!}

> type Peg = String
> type Move = (Peg, Peg)
>
> hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
> hanoi 0 _ _ _ = []
> hanoi 1 a b _ = [(a, b)]
> hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a
> 
> main :: IO ()
> main = do
>     print (hanoi 5 "a" "b" "c")

\textit{And now for some italic stuff}
