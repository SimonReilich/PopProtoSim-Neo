{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Protocols.Combined where

import Data.List
import Data.Text hiding (head, length, replace)
import Protocols
import Protocols.Modulo hiding (delta, output, stringify, test)
import Text.Colour
import Util

delta :: Int -> Int -> (Int, [Int], Int) -> (Int, [Int], Int) -> ((Int, [Int], Int), (Int, [Int], Int))
delta m t (l1, s1, h1) (l2, s2, h2)
  | l1 == l2 =
      if l1 <= 2 * m + 1
        then
          let newSave s = Util.replace l1 ((s1 !! l1 + s2 !! l1) `mod` m) (Util.replace (l1 + 1) 1 s)
           in ((l1, newSave s1, min t (max (max h1 h2) (l1 + 2))), (l1 + 1, newSave s2, min t (max (max h1 h2) (l1 + 2))))
        else
          if l1 < t
            then ((l1, s1, min t (max (l1 + 1) (max h1 h2))), (l1 + 1, s2, min t (max (l1 + 1) (max h1 h2))))
            else ((l1, s1, min t (max h1 h2)), (l2, s2, min t (max h1 h2)))
  | l1 <= 2 * m + 1 =
      if l2 <= 2 * m + 1
        then
          let newSave s = Util.replace l1 (s1 !! l1) (Util.replace l2 (s2 !! l2) s)
           in ((l1, newSave s1, min t (max h1 h2)), (l2, newSave s2, min t (max h1 h2)))
        else ((l1, s1, min t (max h1 h2)), (l2, Util.replace l1 (s1 !! l1) s2, min t (max h1 h2)))
  | l2 <= 2 * m + 1 = ((l1, Util.replace l2 (s2 !! l2) s1, min t (max h1 h2)), (l2, s2, min t (max h1 h2)))
  | otherwise = ((l1, s1, min t (max h1 h2)), (l2, s2, min t (max h1 h2)))

stringify :: Int -> Int -> (Int, [Int], Int) -> Chunk
stringify m t (l, s, h) =
  let (r, g, b) = Util.hsl2Rgb (fromIntegral (h `mod` m) / fromIntegral m) (0.75 * (fromIntegral (min h t) / fromIntegral t) + 0.25) 0.5
   in fore (colourRGB r g b) (chunk (pack ("(" ++ show l ++ ";" ++ Util.vec2String s ++ ";" ++ show h ++ ")")))

output :: Int -> Int -> (Int, [Int], Int) -> ((Int, Int), Colour)
output m t (_, s, h) =
  let (r, g, b) = Util.hsl2Rgb (fromIntegral (h `mod` m) / fromIntegral m) (0.75 * (fromIntegral (min h t) / fromIntegral t) + 0.25) 0.5
   in if h < 2 * m + 1
        then ((h `mod` m, min h t), colourRGB r g b)
        else ((mostCommon (case Data.List.foldl (\(i, acc) a -> (i + 1, ((a + i) `mod` m) : acc)) (0, []) s of (_, res) -> res), min h t), colourRGB r g b)

test :: Int -> Int -> [Int] -> (Int, Int) -> Int
test m t [x0] (rMod, rCut) =
  if x0 `mod` m == rMod && min t x0 == rCut
    then 0
    else 1 + test m t [x0 - 1] (rMod, rCut)
test _ _ _ _ =
  0

get :: Int -> Int -> Protocols.Protocol (Int, [Int], Int) (Int, Int)
get m t = (Protocols.Modulo.input m, delta m (max t (2 * m + 2)), stringify m t, output m t, test m t)