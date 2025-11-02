{-# LANGUAGE RankNTypes #-}
module Util where

import Control.Concurrent
import Data.List
import Data.Ord
import Data.Text hiding (any, find, head, length, replace, replicate, show)
import Data.Word
import Protocols
import Text.Colour

printConfig :: (Eq a, Show a) => Configuration a -> Int -> Int -> (a -> Chunk) -> Int -> IO ()
printConfig c s1 s2 stringify delay =
  threadDelay delay
    >> let helper [] _ _ = [chunk (pack "\n")]
           helper ((b, s) : states) a1 a2
             | not b = chunk (pack "  ") : italic (faint (stringify s)) : helper states (a1 - 1) (a2 - 1)
             | a1 == 0 || a2 == 0 = chunk (pack "  ") : back (colorRGB 255 255 255) (slowBlinking (bold (stringify s))) : helper states (a1 - 1) (a2 - 1)
             | otherwise = chunk (pack "  ") : stringify s : helper states (a1 - 1) (a2 - 1)
        in putChunksUtf8With With24BitColours (helper c s1 s2)

replace :: Int -> a -> [a] -> [a]
replace _ _ [] =
  []
replace i e (x : xs) =
  if i <= 0
    then e : xs
    else x : replace (i - 1) e xs

mostCommon :: (Ord a) => [a] -> a
mostCommon list =
  head (Data.List.maximumBy (Data.Ord.comparing length) (Data.List.group (Data.List.sort list)))

hsl2Rgb :: Float -> Float -> Float -> (Word8, Word8, Word8)
hsl2Rgb h s l =
  if s == 0
    then (round (l + 255), round (l + 255), round (l + 255))
    else
      let q = if l < 0.5 then l * (1 + s) else l + s - l * s
       in let p = 2 * l - q
           in ( round (hue2Rgb p q (h + 1 / 3) * 255),
                round (hue2Rgb p q h * 255),
                round (hue2Rgb p q (h - 1 / 3) * 255)
              )

hue2Rgb :: Float -> Float -> Float -> Float
hue2Rgb p q t
  | t < 0 = hue2Rgb p q (t + 1)
  | t > 1 = hue2Rgb p q (t - 1)
  | t < 1 / 6 = p + (q - p) * 6 * t
  | t < 1 / 2 = q
  | t < 2 / 3 = p + (q - p) * (2 / 3 - t) * 6
  | otherwise = p

vec2String :: (Show a) => [a] -> String
vec2String [] =
  "()"
vec2String (x : xs) =
  let helper [] = ")"
      helper (y : ys) =
        "|" ++ show y ++ helper ys
   in "(" ++ show x ++ helper xs