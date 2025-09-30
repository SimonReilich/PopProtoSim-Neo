module Protocols.Cut where

import Protocols

get :: Int -> Protocols.Input (Int, Int)
get x0 =
  let input :: Int -> Protocols.Configuration (Int, Int)
      input x =
        if x <= 0
          then []
          else (1, 1) : input (x - 1)
   in let delta :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
          delta (l1, h1) (l2, h2) =
            if l1 == l2
              then ((l1, max (l1 + 1) (max h1 h2)), (l1 + 1, max (l1 + 1) (max h1 h2)))
              else ((l1, max h1 h2), (l2, max h1 h2))
       in let stringify :: (Int, Int) -> String
              stringify (l, h) =
                "(" ++ show l ++ "; " ++ show h ++ ")"
           in let output :: (Int, Int) -> Int
                  output (_, h) =
                    h
               in (input x0, delta, stringify, output)