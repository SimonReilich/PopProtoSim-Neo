module Protocols.Modulo where

import Util

input :: [Int] -> [Int] -> Util.Configuration (Int, [Int], Int)
input (m : _) (x0 : _) = 
    if x0 <= 0 
        then []
        else (1, [], 1) : input [m] [x0 - 1]
input _ [] = []
input [] _ = []

delta :: (Int, [Int], Int) -> (Int, [Int], Int) -> ((Int, [Int], Int), (Int, [Int], Int))
delta (l1, c1, h1) (l2, c2, h2) =
    ((l1, c1, h1), (l2, c2, h2))


stringify :: (Int, [Int], Int) -> String
stringify (l, cs, h) =
    "(" ++ (show l) ++ "; " ++ (show h) ++ ")"

output :: (Int, [Int], Int) -> Int
output (_, cs, h) =
    h
    
get :: [Int] -> [Int] -> Util.Input (Int, [Int], Int)
get as xs = (input as xs, delta, stringify, output)