module Util where

import Protocols
import Data.List (group, maximumBy)
import qualified Data.Ord

printConfig :: Protocols.Configuration a -> Int -> Int -> (a -> String) -> String
printConfig [] _ _ _ = " |"
printConfig (s : states) a1 a2 stringify =
  if a1 == 0 || a2 == 0
    then "| *" ++ stringify s ++ "* " ++ printConfig states (a1 - 1) (a2 - 1) stringify
    else "|  " ++ stringify s ++ "  " ++ printConfig states (a1 - 1) (a2 - 1) stringify

hash :: Protocol a -> Int
hash (c, _, _, o) =
  let helper [] _ _ = 0
      helper (s : states) output n =
        output s ^ n + helper states output (n + 1)
   in helper c o (1 :: Integer)

replace :: Int -> a -> Protocols.Configuration a -> Protocols.Configuration a
replace _ _ [] =
  []
replace i e (x : xs) =
  if i <= 0
    then e : xs
    else x : replace (i - 1) e xs

intArrayToString :: [Int] -> String
intArrayToString list =
  let 
    helper [] = "]"
    helper [x] = show x ++ "]"
    helper (x : xs) = show x ++ "; " ++ helper xs
  in "[" ++ helper list

mostCommon :: (Eq a) => [a] -> a
mostCommon list =
  head (Data.List.maximumBy (Data.Ord.comparing length) (Data.List.group list))