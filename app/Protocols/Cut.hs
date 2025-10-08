module Protocols.Cut where

import Protocols

input :: Int -> (Int, Int)
input _ = (1, 1)

delta :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
delta (l1, h1) (l2, h2) =
  if l1 == l2
    then ((l1, max (l1 + 1) (max h1 h2)), (l1 + 1, max (l1 + 1) (max h1 h2)))
    else ((l1, max h1 h2), (l2, max h1 h2))

stringify :: (Int, Int) -> String
stringify (l, h) =
  "(" ++ show l ++ "; " ++ show h ++ ")"

output :: (Int, Int) -> String
output (_, h) = show h

get :: Protocols.Protocol (Int, Int)
get = (input, delta, stringify, output)