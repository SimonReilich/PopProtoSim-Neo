module Protocols.Cut where

import Util

input :: [Int] -> [Int] -> Util.Configuration (Int, Int)
input _ (x0 : _) = 
    if x0 <= 0 
        then []
        else (1, 1) : input [x0 - 1]
input _ [] = []

delta :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
delta (l1, h1) (l2, h2) =
    if l1 == l2 
        then ((l1, max (l1 + 1) (max h1 h2)), (l1 + 1, max (l1 + 1) (max h1 h2)))
        else ((l1, max h1 h2), (l2, max h1 h2))

stringify :: (Int, Int) -> String
stringify (l, h) =
    "(" ++ (show l) ++ "; " ++ (show h) ++ ")"

output :: (Int, Int) -> Int
output (_, h) =
    h
    
get :: [Int] -> [Int] -> Util.Input (Int, Int)
get as xs = (input as xs, delta, stringify, output)