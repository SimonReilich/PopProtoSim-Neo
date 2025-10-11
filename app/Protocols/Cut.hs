module Protocols.Cut where

import Protocols
import Data.Text hiding (length, head, replace)
import Text.Colour
import Util

input :: Int -> (Int, Int)
input _ = (1, 1)

delta :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
delta (l1, h1) (l2, h2) =
  if l1 == l2
    then ((l1, max (l1 + 1) (max h1 h2)), (l1 + 1, max (l1 + 1) (max h1 h2)))
    else ((l1, max h1 h2), (l2, max h1 h2))

stringify :: Int -> (Int, Int) -> Chunk
stringify t (l, h) =
  let (r, g, b) = Util.hslToRgb (fromIntegral h / fromIntegral t) 1.0 0.5
  in fore (colourRGB r g b) (chunk (pack ("(" ++ show l ++ ";" ++ show h ++ ")")))

output :: Int -> (Int, Int) -> Chunk
output t (_, h) =
  let (r, g, b) = Util.hslToRgb (fromIntegral h / fromIntegral t) 1.0 0.5
  in fore (colourRGB r g b) (chunk (pack (show h)))

get :: Int -> Protocols.Protocol (Int, Int)
get t = (input, delta, stringify t, output t)