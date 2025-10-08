module Protocols.Combined where

import Data.List
import Util
import Protocols
import Protocols.Modulo

output :: Int -> (Int, [Int], Int) -> String
output m (_, s, h) =
  if h < 2 * m + 1
    then "(" ++ show (h `mod` m) ++ "; " ++ show h ++ ")"
    else "(" ++ show (mostCommon (case Data.List.foldl (\(i, acc) a -> (i + 1, ((a + i) `mod` m) : acc)) (0, []) s of (_, res) -> res)) ++ "; " ++ show h ++ ")"

get :: Int -> Protocols.Protocol (Int, [Int], Int)
get m = (Protocols.Modulo.input m, Protocols.Modulo.delta m, Protocols.Modulo.stringify, Protocols.Combined.output m)