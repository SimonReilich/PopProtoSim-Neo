{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Protocols.Tuple where

import Protocols
import Data.Either
import Text.Colour

input :: InputAssignment a -> InputAssignment c -> b -> d -> Int -> Either (a, d) (c, b)
input i1 i2 n1 n2 x = 
    if x `mod` 2 == 0 
        then Left (i1 (x `div` 2), n2)
        else Right (i2 (x `div` 2), n1)

delta :: Eq b => Eq d => TransitionFunction a -> TransitionFunction c -> b -> d -> OutputFunction a b -> OutputFunction c d -> Either (a, d) (c, b) -> Either (a, d) (c, b) -> (Either (a, d) (c, b), Either (a, d) (c, b))
delta d1 d2 n1 n2 o1 o2 (Left(q1, s1)) (Left(q2, s2)) =
    if s1 == s2
        then let (q1', q2') = d1 q1 q2
        in (Left (q1', s1), Left (q2', s2))
        else let (q1', q2') = d1 q1 q2
        in (Left (q1', n2), Left (q2', n2))
delta d1 d2 n1 n2 o1 o2 (Right (q1, s1)) (Right (q2, s2)) =
    if s1 == s2
        then let (q1', q2') = d2 q1 q2
        in (Right (q1', s1), Right (q2', s2))
        else let (q1', q2') = d2 q1 q2
        in (Right (q1', n1), Right (q2', n1))
delta d1 d2 n1 n2 o1 o2 (Left (q1, _)) (Right (q2, _)) =
    let (s1, _) = o2 q2
    in let (s2, _) = o1 q1
    in (Left (q1, s1), Right (q2, s2))
delta d1 d2 n1 n2 o1 o2 (Right (q1, _)) (Left (q2, _)) =
    let (s1, _) = o1 q2
    in let (s2, _) = o2 q1
    in (Right (q1, s1), Left (q2, s2))

stringify :: ToChunkFunction a -> ToChunkFunction c -> Either (a, d) (c, b) -> Chunk
stringify s1 _ (Left (q, s)) =
    s1 q
stringify _ s2 (Right (q, s)) =
    s2 q

output :: OutputFunction a b -> OutputFunction c d -> Either (a, d) (c, b) -> ((b, d), Colour)
output o1 o2 (Left (q, s)) =
    let (o, colour) = o1 q
    in ((o, s), colour)
output o1 o2 (Right (q, s)) =
    let (o, colour) = o2 q
    in ((s, o), colour)

test :: TestFunction b -> TestFunction d -> [Int] -> (b, d) -> Int
test _ _ _ _ =
    0

get :: Eq b => Eq d => Show b => Show d => Protocols.Protocol a b -> Protocols.Protocol c d -> b -> d -> Protocols.Protocol (Either (a, d) (c, b)) (b, d)
get (i1, d1, s1, o1, t1) (i2, d2, s2, o2, t2) n1 n2 = (input i1 i2 n1 n2, delta d1 d2 n1 n2 o1 o2, stringify s1 s2, output o1 o2, test t1 t2)