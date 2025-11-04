{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Protocols.Cut where

import Data.Text hiding (head, length, replace, show)
import Protocols
import Text.Colour
import Util

input :: Int -> (Int, Int)
input _ = (1, 1)

delta :: Int -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
delta t (l1, h1) (l2, h2) =
  if l1 == l2
    then ((l1, min t (max (l1 + 1) (max h1 h2))), (l1 + 1, min t (max (l1 + 1) (max h1 h2))))
    else ((l1, min t (max h1 h2)), (l2, min t (max h1 h2)))

stringify :: Int -> (Int, Int) -> Chunk
stringify t (l, h) =
  let (r, g, b) = Util.hsl2Rgb (fromIntegral h / fromIntegral t) 1.0 0.5
   in fore (colourRGB r g b) (chunk (pack ("(" ++ show l ++ ";" ++ show h ++ ")")))

output :: Int -> (Int, Int) -> (Int, Colour)
output t (_, h) =
  let (r, g, b) = Util.hsl2Rgb (fromIntegral (min h t) / fromIntegral t) 1.0 0.5
   in (min h t, colourRGB r g b)

test :: Int -> [Int] -> Int -> Int
test t [x0] result =
  if min t x0 == result
    then 0
    else 1 + test t [x0 - 1] result
test _ _ _ =
  0

get :: Int -> Protocols.Protocol (Int, Int) Int
get t = Protocol (input) (delta t) (stringify t) (output t) (test t) (\(x:_) -> min (max x 0) t)