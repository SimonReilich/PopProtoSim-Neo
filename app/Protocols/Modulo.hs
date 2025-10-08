module Protocols.Modulo where

import Data.List
import Protocols
import Util

input :: Int -> Int -> Protocols.Configuration (Int, [Int], Int)
input m x =
  if x <= 0
    then []
    else (True, (0, 1 : replicate (2 * m) 0, 1)) : input m (x - 1)

delta :: Int -> (Int, [Int], Int) -> (Int, [Int], Int) -> ((Int, [Int], Int), (Int, [Int], Int))
delta m (l1, s1, h1) (l2, s2, h2)
  | l1 == l2 =
      if l1 <= 2 * m + 1
        then
          let newSave s = Util.replace l1 ((s1 !! l1 + s2 !! l1) `mod` m) (Util.replace (l1 + 1) 1 s)
           in ((l1, newSave s1, max (max h1 h2) (l1 + 2)), (l1 + 1, newSave s2, max (max h1 h2) (l1 + 2)))
        else ((l1, s1, max h1 h2), (l2, s2, max h1 h2))
  | l1 <= 2 * m + 1 =
      if l2 <= 2 * m + 1
        then
          let newSave s = Util.replace l1 (s1 !! l1) (Util.replace l2 (s2 !! l2) s)
           in ((l1, newSave s1, max h1 h2), (l2, newSave s2, max h1 h2))
        else ((l1, s1, max h1 h2), (l2, Util.replace l1 (s1 !! l1) s2, max h1 h2))
  | l2 <= 2 * m + 1 = ((l1, Util.replace l2 (s2 !! l2) s1, max h1 h2), (l2, s2, max h1 h2))
  | otherwise = ((l1, s1, max h1 h2), (l2, s2, max h1 h2))

stringify :: (Int, [Int], Int) -> String
stringify (l, s, h) =
  "(" ++ show l ++ "; " ++ Util.intArrayToString s ++ "; " ++ show h ++ ")"

output :: Int -> (Int, [Int], Int) -> String
output m (_, s, h) =
  if h < 2 * m + 1
    then show (h `mod` m)
    else show (mostCommon (case Data.List.foldl (\(i, acc) a -> (i + 1, ((a + i) `mod` m) : acc)) (0, []) s of (_, res) -> res))

get :: Int -> Int -> Sniper (Int, [Int], Int) a -> Protocols.Protocol (Int, [Int], Int) a
get m x0 sn = (input m x0, delta m, stringify, output m, sn)