{-# LANGUAGE RankNTypes #-}

module Protocols where

import Text.Colour

type InputAssignment a = Int -> a

type TransitionFunction a = a -> a -> (a, a)

type ToChunkFunction a = a -> Chunk

type OutputFunction a b = a -> (b, Colour)

type TestFunction b = [Int] -> b -> Int

type Configuration a = [(Bool, a)]

type Protocol a b = (Eq b) => (Show b) => (InputAssignment a, TransitionFunction a, ToChunkFunction a, OutputFunction a b, TestFunction b)

deltaWrapper :: (a -> a -> (a, a)) -> ((Bool, a) -> (Bool, a) -> ((Bool, a), (Bool, a)))
deltaWrapper d (b1, s1) (b2, s2) =
  if b1 && b2
    then
      let (s1n, s2n) = d s1 s2
       in ((b1, s1n), (b2, s2n))
    else ((b1, s1), (b2, s2))

getInitial :: (Eq b) => (Show b) => Protocol a b -> [Int] -> Configuration a
getInitial (mapping, _, _, _, _) input =
  let helper acc _ [] = acc
      helper acc i (x : xs) =
        if x <= 0
          then helper acc (i + 1) xs
          else helper ((True, mapping i) : acc) i ((x - 1) : xs)
   in helper [] 0 input