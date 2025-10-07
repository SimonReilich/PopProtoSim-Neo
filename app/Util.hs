module Util where

import Data.List
import Data.Ord
import Protocols

printConfig :: Protocols.Configuration a -> Int -> Int -> (a -> String) -> String
printConfig [] _ _ _ = " |"
printConfig ((b, s) : states) a1 a2 stringify
  | not b =              "| -" ++ stringify s ++ "- " ++ printConfig states (a1 - 1) (a2 - 1) stringify
  | a1 == 0 || a2 == 0 = "| *" ++ stringify s ++ "* " ++ printConfig states (a1 - 1) (a2 - 1) stringify
  | otherwise =          "|  " ++ stringify s ++ "  " ++ printConfig states (a1 - 1) (a2 - 1) stringify

replace :: Int -> a -> [a] -> [a]
replace _ _ [] =
  []
replace i e (x : xs) =
  if i <= 0
    then e : xs
    else x : replace (i - 1) e xs

intArrayToString :: [Int] -> String
intArrayToString list =
  let helper [] = "]"
      helper [x] = show x ++ "]"
      helper (x : xs) = show x ++ "; " ++ helper xs
   in "[" ++ helper list

mostCommon :: (Eq a) => [a] -> a
mostCommon list =
  head (Data.List.maximumBy (Data.Ord.comparing length) (Data.List.group list))