module Snipers where

import Data.List
import System.Random
import Protocols

type Sniper a s = (s, Configuration a -> s -> (s, Maybe Int))

randomSniper :: Int -> Float -> Sniper a StdGen
randomSniper seed chance =
  let snipe config gen =
        if length (Data.List.filter fst config) >= 2
          then
            let (res, gen1) =
                  randomR (0.0, 1.0) gen
             in if res < chance
                  then
                    let (agent, gen2) = randomR (0, length config - 1) gen1
                     in (gen2, Just agent)
                  else (gen1, Nothing)
          else (gen, Nothing)
   in (mkStdGen seed, snipe)

noSniper :: Sniper a ()
noSniper =
  ((), \_ _ -> ((), Nothing))