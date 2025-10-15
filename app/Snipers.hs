module Snipers where

import Data.List
import System.Random
import Protocols
import System.IO

type Sniper a s = (s, Configuration a -> s -> IO (s, Maybe Int))

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
                     in return (gen2, Just agent)
                  else return (gen1, Nothing)
          else return (gen, Nothing)
   in (mkStdGen seed, snipe)

manualSniper :: Sniper a ()
manualSniper =
  let snipe config _ =
        if null config
          then return ((), Nothing)
          else do
            putStr "\nAgent to be sniped: "
            hFlush stdout
            agent <- readLn :: IO Int
            let r = ((), if agent > 0 && agent <= length config then Just (agent - 1) else Nothing)
            putStrLn ""
            return r
  in ((), snipe)

noSniper :: Sniper a ()
noSniper =
  ((), \_ _ -> return ((), Nothing))