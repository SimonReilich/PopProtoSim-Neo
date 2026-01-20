{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Protocols.Majority where

import Data.Text hiding (head, length, replace, show)
import Protocols
import Text.Colour
import Util

data State = Y Bool | T Bool | N Bool

instance Eq State where
  (==) (Y a1) (Y a2) = a1 == a2
  (==) (T a1) (T a2) = a1 == a2
  (==) (N a1) (N a2) = a1 == a2
  (==) _ _ = False

instance Show State where
  show (Y a) = (if a then "A" else "P") ++ "_y"
  show (T a) = (if a then "A" else "P") ++ "_t"
  show (N a) = (if a then "A" else "P") ++ "_n"

input :: Int -> State
input x = if x `mod` 2 == 0 then Y True else N True

active :: State -> Bool
active (Y a) = a
active (T a) = a
active (N a) = a

delta :: State -> State -> (State, State)
delta (Y True) (T True) = (Y True, Y False)
delta (Y True) (N True) = (T True, T False)
delta (T True) (N True) = (N True, N False)
delta (T True) (T True) = (T True, T False)
delta (Y True) second =
  if (active second)
    then (Y True, second)
    else (Y True, Y False)
delta (T True) second =
  if (active second)
    then (T True, second)
    else (T True, T False)
delta (N True) second =
  if (active second)
    then (N True, second)
    else (N True, N False)
delta first second = (first, second)

stringify :: State -> Chunk
stringify (Y a) =
  let (r, g, b) = Util.hsl2Rgb 0.3 1.0 0.5
   in fore (colourRGB r g b) (chunk (pack (if a then "A_y" else "P_y")))
stringify (T a) =
  let (r, g, b) = Util.hsl2Rgb 0.5 1.0 0.5
   in fore (colourRGB r g b) (chunk (pack (if a then "A_t" else "P_t")))
stringify (N a) =
  let (r, g, b) = Util.hsl2Rgb 0.0 1.0 0.5
   in fore (colourRGB r g b) (chunk (pack (if a then "A_n" else "P_n")))

output :: State -> (Maybe Bool, Colour)
output (Y _) =
  let (r, g, b) = Util.hsl2Rgb 0.3 1.0 0.5
   in (Just True, colourRGB r g b)
output (T _) =
  let (r, g, b) = Util.hsl2Rgb 0.5 1.0 0.5
   in (Nothing, colourRGB r g b)
output (N _) =
  let (r, g, b) = Util.hsl2Rgb 0.0 1.0 0.5
   in (Just False, colourRGB r g b)

test :: [Int] -> (Maybe Bool) -> Int
test [x, y] (Just result) =
  if (x >= y) == result
    then 0
    else
      if result
        then 1 + test [x, y - 1] (Just result)
        else 1 + test [x - 1, y] (Just result)
test [x, y] Nothing =
  if x >= y
    then x - y
    else y - x
test _ _ =
  0

get :: Protocols.Protocol State (Maybe Bool)
get = Protocol (input) (delta) (stringify) (output) (test) (\(x : y : _) -> if x == y then Nothing else Just (x >= y))
