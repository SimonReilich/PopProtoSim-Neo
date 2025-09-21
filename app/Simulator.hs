module Simulator where

import Config
import System.Random

run :: Config.Input a -> Int
run (states, delta, output) = 
    if stable (states, delta, output)
        then output (states!!0)
    else let
        a1 = getRandom (size states) -1
    in let
        a2 = getRandom (size states) a1
    in let
        (q1, q2) = delta (states!!a1) (states!!a2)
    in run (repl a1 q1 (repl a2 q2 states), delta, output)

stable :: Config.Input a -> Bool
stable (states, delta, output) =
    size (filter (\a -> output a == output (states!!0)) states) == size states

size :: [a] -> Int
size [] =
    0
size (x:xs) =
    1 + (size xs)

repl :: Int -> a -> [a] -> [a]
repl i e [] =
    []
repl i e (x:xs) =
    if i <= 0
        then e:xs
        else x:(repl (i - 1) e xs)

getRandom :: Int -> Int -> Int
getRandom m n =
    let 
        res = randomRIO (0, m)
    in if res == n
        then getRandom m n
        else res
