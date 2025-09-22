module Util where

type Configuration a = [a]
type Input a = (Configuration a, a -> a -> (a, a), a -> String, a -> Int)

hash :: Input a -> Int
hash (states, delta, stringify, output) =
    let 
        helper [] output n = 0
        helper (s:states) output n =
            (output s)^n + helper states output (n + 1)
    in helper states output 1

repl :: Int -> a -> [a] -> [a]
repl _ _ [] =
    []
repl i e (x:xs) =
    if i <= 0
        then e:xs
        else x:repl (i - 1) e xs