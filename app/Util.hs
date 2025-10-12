module Util where

import Data.List
import Data.Ord
import Protocols
import Text.Colour
import Data.Word
import Data.Text hiding (length, head, replace, replicate, find, any)
import Control.Concurrent

printConfig :: Configuration a -> Int -> Int -> (a -> Chunk) -> Int -> IO ()
printConfig s s1 s2 stringify delay =
  threadDelay delay >>
  let
    helper [] _ _ = [chunk (pack "\n")]
    helper ((b, s) : states) a1 a2
        | not b =              chunk (pack "  ") : italic (faint (stringify s)) : helper states (a1 - 1) (a2 - 1)
        | a1 == 0 || a2 == 0 = chunk (pack "  ") : back (colorRGB 255 255 255) (slowBlinking (bold (stringify s))) : helper states (a1 - 1) (a2 - 1)
        | otherwise =          chunk (pack "  ") : stringify s : helper states (a1 - 1) (a2 - 1)
  in putChunksUtf8With With24BitColours (helper s s1 s2)

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
      helper (x : xs) = show x ++ ";" ++ helper xs
   in "[" ++ helper list

mostCommon :: (Eq a) => [a] -> a
mostCommon list =
  head (Data.List.maximumBy (Data.Ord.comparing length) (Data.List.group list))

hslToRgb :: Float -> Float -> Float -> (Word8, Word8, Word8)
hslToRgb h s l =
  if s == 0
    then (round (l + 255), round (l + 255), round (l + 255))
    else
      let q = if l < 0.5 then l * (1 + s) else l + s - l * s
      in
        let p = 2 * l - q
        in (
          round (hueToRgb p q (h + 1/3) * 255),
          round (hueToRgb p q h * 255),
          round (hueToRgb p q (h - 1/3) * 255)
        )

hueToRgb :: Float -> Float -> Float -> Float
hueToRgb p q t
  | t < 0     = hueToRgb p q (t + 1)
  | t > 1     = hueToRgb p q (t - 1)
  | t < 1/6   = p + (q - p) * 6 * t
  | t < 1/2   = q
  | t < 2/3   = p + (q - p) * (2/3 - t) * 6
  | otherwise = p

vec2String :: (Show a) => [a] -> String
vec2String [] =
  "()"
vec2String (x : xs) =
  let 
    helper [] = ")"
    helper (x : xs) =
      "|" ++ show x ++ helper xs
  in "(" ++ show x ++ helper xs