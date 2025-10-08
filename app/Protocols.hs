module Protocols where

type Configuration a = [(Bool, a)]

type Protocol a = (Int -> a, a -> a -> (a, a), a -> String, a -> String)

deltaWrapper :: (a -> a -> (a, a)) -> ((Bool, a) -> (Bool, a) -> ((Bool, a), (Bool, a)))
deltaWrapper d (b1, s1) (b2, s2) =
  if b1 && b2
    then
      let (s1n, s2n) = d s1 s2
       in ((b1, s1n), (b2, s2n))
    else ((b1, s1), (b2, s2))

getInitial :: Protocol a -> [Int] -> Configuration a
getInitial (mapping, _, _, _) input =
  let 
    helper acc _ [] = acc
    helper acc i (x : xs) =
      if x <= 0
        then helper acc (i + 1) xs
        else helper ((True, mapping i) : acc) i ((x - 1) : xs)
  in helper [] 0 input