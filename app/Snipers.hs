module Snipers where

import Data.List
import System.Random
import Protocols

type Sniper a s = (s, Configuration a -> s -> (s, Maybe Int))

randomSniper :: Float -> Sniper a StdGen
randomSniper chance =
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
   in (mkStdGen 0, snipe)

maxSniper :: Sniper a b -> Int -> Sniper a (Int, b)
maxSniper (innerState, sniping) max =
  let snipe config (number, innerState) =
        if number == max 
      then ((number, innerState), Nothing)
      else case sniping config innerState of
        (newInnerState, Nothing) -> ((number, newInnerState), Nothing)
        (newInnerState, Just i)  -> ((number + 1, newInnerState), Just i)
  in ((0, innerState), snipe)

noSniper :: Sniper a ()
noSniper =
  ((), \_ _ -> ((), Nothing))