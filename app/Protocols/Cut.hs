module Protocols.Cut where

import Protocols

input :: Int -> Protocols.Configuration (Int, Int)
input x =
  if x <= 0
    then []
    else (1, 1) : input (x - 1)

delta :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
delta (l1, h1) (l2, h2) =
  if l1 == l2
    then ((l1, max (l1 + 1) (max h1 h2)), (l1 + 1, max (l1 + 1) (max h1 h2)))
    else ((l1, max h1 h2), (l2, max h1 h2))

stringify :: (Int, Int) -> String
stringify (l, h) =
  "(" ++ show l ++ "; " ++ show h ++ ")"

output :: (Int, Int) -> Int
output (_, h) = h

get :: Int -> Protocols.Protocol (Int, Int)
get x0 = (input x0, delta, stringify, output)