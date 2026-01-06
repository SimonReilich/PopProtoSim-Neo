{-# LANGUAGE RankNTypes #-}

module Protocols where

import Data.Data (Typeable)
import Text.Colour

type InputAssignment a = (Eq a, Show a) => Int -> a

type TransitionFunction a = (Eq a, Show a) => a -> a -> (a, a)

type ToChunkFunction a = (Eq a, Show a) => a -> Chunk

type OutputFunction a b = (Eq a, Show a, Eq b, Show b) => a -> (b, Colour)

type TestFunction b = (Eq b, Show b) => [Int] -> b -> Int

type Configuration a = (Eq a, Show a) => [(Bool, a)]

data Protocol a b = Protocol (InputAssignment a) (TransitionFunction a) (ToChunkFunction a) (OutputFunction a b) (TestFunction b) ([Int] -> b)
  deriving (Typeable)

deltaWrapper :: (a -> a -> (a, a)) -> ((Bool, a) -> (Bool, a) -> ((Bool, a), (Bool, a)))
deltaWrapper d (b1, s1) (b2, s2) =
  if b1 && b2
    then
      let (s1n, s2n) = d s1 s2
       in ((b1, s1n), (b2, s2n))
    else ((b1, s1), (b2, s2))

getInitial :: (Eq b) => (Show b) => Protocol a b -> [Int] -> Configuration a
getInitial (Protocol mapping _ _ _ _ _) input =
  let helper acc _ [] = acc
      helper acc i (x : xs) =
        if x <= 0
          then helper acc (i + 1) xs
          else helper ((True, mapping i) : acc) i ((x - 1) : xs)
   in helper [] 0 input
