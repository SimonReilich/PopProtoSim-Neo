module Protocols where

import System.Random

type Configuration a = [(Bool, a)]

type Protocol a b = (Configuration a, a -> a -> (a, a), a -> String, a -> Int, Sniper a b)

deltaWrapper :: (a -> a -> (a, a)) -> ((Bool, a) -> (Bool, a) -> ((Bool, a), (Bool, a)))
deltaWrapper d (b1, s1) (b2, s2) =
  if b1 && b2
    then
      let (s1n, s2n) = d s1 s2
       in ((b1, s1n), (b2, s2n))
    else ((b1, s1), (b2, s2))

type Sniper a b = (b, Configuration a -> b -> (b, Maybe Int))

randomSniper :: Float -> Sniper a StdGen
randomSniper chance =
  let snipe config gen =
        let (res, gen1) =
              randomR (0.0, 1.0) gen
         in if res < chance
              then
                let (agent, gen2) = randomR (0, length config - 1) gen1
                 in (gen2, Just agent)
              else (gen1, Nothing)
   in (mkStdGen 0, snipe)

noSniper :: Sniper a ()
noSniper =
  ((), \_ _ -> ((), Nothing))