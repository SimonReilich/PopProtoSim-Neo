{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Protocols.Modulo where

import Data.List
import Data.Text hiding (head, length, replace, replicate, show)
import Protocols
import Text.Colour
import Util

input :: Int -> Int -> (Int, [Int], Int)
input m _ = (0, 1 : replicate (m ^ 2 - 1) 0, 1)

delta :: Int -> (Int, [Int], Int) -> (Int, [Int], Int) -> ((Int, [Int], Int), (Int, [Int], Int))
delta m (l1, s1, h1) (l2, s2, h2)
  | l1 == l2 =
      if l1 <= m ^ 2
        then
          let newSave = Util.replace l1 ((s1 !! l1 + s2 !! l1) `mod` m) (Util.replace (l1 + 1) 1 (agree s1 s2))
           in ((l1, newSave, max (max h1 h2) (l1 + 2)), (l1 + 1, newSave, max (max h1 h2) (l1 + 2)))
        else ((l1, s1, max h1 h2), (l2, s2, max h1 h2))
  | l1 <= m ^ 2 =
      if l2 <= m ^ 2
        then
          let newSave = Util.replace l1 (s1 !! l1) (Util.replace l2 (s2 !! l2) (agree s1 s2))
           in ((l1, newSave, max h1 h2), (l2, newSave, max h1 h2))
        else ((l1, s1, max h1 h2), (l2, Util.replace l1 (s1 !! l1) s2, max h1 h2))
  | l2 <= m ^ 2 = ((l1, Util.replace l2 (s2 !! l2) s1, max h1 h2), (l2, s2, max h1 h2))
  | otherwise = ((l1, s1, max h1 h2), (l2, s2, max h1 h2))

stringify :: Int -> (Int, [Int], Int) -> Chunk
stringify m (l, s, h) =
  let (r, g, b) = Util.hsl2Rgb (fromIntegral (if h < m ^ 2 then h `mod` m else mostCommon (case Data.List.foldl (\(i, acc) a -> (i + 1, ((a + i) `mod` m) : acc)) (0, []) s of (_, res) -> res) `mod` m) / fromIntegral m) 1.0 0.5
   in fore (colourRGB r g b) (chunk (pack ("(" ++ show l ++ ";" ++ Util.vec2String s ++ ";" ++ show h ++ ")")))

output :: Int -> (Int, [Int], Int) -> (Int, Colour)
output m (_, s, h) =
  let (r, g, b) = Util.hsl2Rgb (fromIntegral (if h < m ^ 2 then h `mod` m else mostCommon (case Data.List.foldl (\(i, acc) a -> (i + 1, ((a + i) `mod` m) : acc)) (0, []) s of (_, res) -> res) `mod` m) / fromIntegral m) 1.0 0.5
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

agree :: [Int] -> [Int] -> [Int]
agree [] _ =
  []
agree _ [] =
  []
agree (v : vs) (u : us) =
  if v == u
    then v : (agree vs us)
    else 0 : (agree vs us)

get :: Int -> Protocols.Protocol (Int, [Int], Int) Int
get m = Protocol (input m) (delta m) (stringify m) (output m) (test m) (\(x:_) -> x `mod` m)