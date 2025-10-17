{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Protocols.Modulo where

import Data.List
import Protocols
import Util
import Data.Text hiding (length, head, replace, replicate)
import Text.Colour

input :: Int -> Int -> (Int, [Int], Int)
input m _ = (0, 1 : replicate (2 * m) 0, 1)

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

stringify :: Int -> (Int, [Int], Int) -> Chunk
stringify m (l, s, h) =
  let (r, g, b) = Util.hsl2Rgb (fromIntegral (h `mod` m) / fromIntegral m) 1.0 0.5
   in fore (colourRGB r g b) (chunk (pack ("(" ++ show l ++ ";" ++ Util.vec2String s ++ ";" ++ show h ++ ")")))

output :: Int -> (Int, [Int], Int) -> (Int, Colour)
output m (_, s, h) =
  let (r, g, b) = Util.hsl2Rgb (fromIntegral (h `mod` m) / fromIntegral m) 1.0 0.5
  in if h < 2 * m + 1
    then (h `mod` m, colourRGB r g b)
    else (mostCommon (case Data.List.foldl (\(i, acc) a -> (i + 1, ((a + i) `mod` m) : acc)) (0, []) s of (_, res) -> res), colourRGB r g b)

test :: Int -> [Int] -> Int -> Int
test m [x0] result =
  if x0 `mod` m == result 
    then 0
    else 1 + test m [x0 - 1] result
test _ _ _ =
  0

get :: Int -> Protocols.Protocol (Int, [Int], Int) Int
get m = (input m, delta m, stringify m, output m, test m)