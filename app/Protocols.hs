module Protocols where

type Configuration a = [(Bool, a)]

type Protocol a = (Configuration a, a -> a -> (a, a), a -> String, a -> Int)

deltaWrapper :: (a -> a -> (a, a)) -> ((Bool, a) -> (Bool, a) -> ((Bool, a), (Bool, a)))
deltaWrapper d (b1, s1) (b2, s2) =
  if b1 && b2
    then
      let (s1n, s2n) = d s1 s2
       in ((b1, s1n), (b2, s2n))
    else ((b1, s1), (b2, s2))