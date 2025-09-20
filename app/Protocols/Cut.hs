module Protocols.Cut where

import Config
import Data.Char

input :: [Int] -> Config.Configuration (Int, Int)
input (x0 : xs) = 
    if x0 <= 0 
        then []
        else (1, 1) : (input [(x0 - 1)])

delta :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
delta (l1, h1) (l2, h2) =
    if l1 == l2 
        then ((l1, max (l1 + 1) (max h1 h2)), (l1 + 1, max (l1 + 1) (max h1 h2)))
        else ((l1, max h1 h2), (l2, max h1 h2))

output :: (Int, Int) -> Int
output (l, h) =
    h
    
get :: [Int] -> Config.Input (Int, Int)
get xs = (input xs, delta, output)