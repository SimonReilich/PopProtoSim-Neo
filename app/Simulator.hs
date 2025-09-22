module Simulator where

import Config
import System.Random

run :: Config.Input a -> Int
run (states, delta, output) = 
    if stable (states, delta, output)
        then output (head states)
    else let
        a1 = selectAgent (-1) (size states)
    in let
        a2 = selectAgent a1 (size states)
    in let
        (q1, q2) = delta (states!!a1) (states!!a2)
    in run (repl a1 q1 (repl a2 q2 states), delta, output)

stable :: Config.Input a -> Bool
stable (states, delta, output) =
    size (filter (\a -> output a == 3) states) == size states

size :: [a] -> Int
size [] =
    0
size (_:xs) =
    1 + size xs

repl :: Int -> a -> [a] -> [a]
repl _ _ [] =
    []
repl i e (x:xs) =
    if i <= 0
        then e:xs
        else x:repl (i - 1) e xs

generator :: StdGen
generator = mkStdGen 42

selectAgent :: Int -> Int -> Int
selectAgent i n = 
    let 
        rnd :: StdGen -> (Int, StdGen)
        rnd = uniformR (0, n - 1)
    in let 
        result g = 
            let 
                (res, gNew) = rnd g
            in if res == i
                then result gNew
                else res
    in result generator

