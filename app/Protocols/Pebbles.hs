{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Protocols.Pebbles where

import Data.Text hiding (head, length, replace, show)
import Protocols
import Text.Colour
import Util

input :: Int -> Int
input _ = 1

delta :: Int -> Int -> Int -> (Int, Int)
delta t l1 l2 =
  if l1 + l2 < t
    then (0, l1 + l2)
    else (t, t)

stringify :: Int -> Int -> Chunk
stringify t l =
  let (r, g, b) = Util.hsl2Rgb (fromIntegral l / fromIntegral t) 1.0 0.5
   in fore (colourRGB r g b) (chunk (pack (show l)))

output :: Int -> Int -> (Bool, Colour)
output t l =
  let (r, g, b) = Util.hsl2Rgb (fromIntegral (min l t) / fromIntegral t) 1.0 0.5
   in (l >= t, colourRGB r g b)

test :: Int -> [Int] -> Bool -> Int
test t [x0] result =
  if (x0 >= t) == result
    then 0
    else 1 + test t [x0 - 1] result
test _ _ _ =
  0

get :: Int -> Protocols.Protocol Int Bool
get t = Protocol (input) (delta t) (stringify t) (output t) (test t) (\(x : _) -> x >= t)
