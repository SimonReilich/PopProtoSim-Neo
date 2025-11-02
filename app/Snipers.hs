{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Snipers where

import Data.List
import Protocols
import System.IO
import System.Random

data Sniper a s = Sniper s (Configuration a -> s -> IO (s, Maybe Int))

randomSniper :: (Eq a, Show a) => Int -> Float -> Sniper a StdGen
randomSniper seed chance =
  let 
    snipe :: (Eq a, Show a) => Configuration a -> StdGen -> IO (StdGen, Maybe Int) 
    snipe config g =
        if length (Data.List.filter fst config) >= 2
          then
            let (res, gn) = randomR (0.0, 1.0) g
             in if res < chance
                  then
                    let helper gen =
                          let (agent, genNew) = randomR (0, length config - 1) gen
                           in if (case config !! agent of (b, _) -> b)
                                then return (genNew, Just agent)
                                else helper genNew
                     in helper gn
                  else return (gn, Nothing)
          else return (g, Nothing)
   in Sniper (mkStdGen seed) snipe

manualSniper :: (Eq a, Show a) => Sniper a ()
manualSniper =
  let 
    snipe :: (Eq a, Show a) => Configuration a -> () -> IO ((), Maybe Int) 
    snipe config _ =
        if null config
          then return ((), Nothing)
          else do
            putStr "\nAgent to be sniped: "
            hFlush stdout
            agent <- readLn :: IO Int
            let r = ((), if agent > 0 && agent <= length config then Just (agent - 1) else Nothing)
            putStrLn ""
            return r
   in Sniper () snipe

noSniper :: (Eq a, Show a) => Sniper a ()
noSniper =
  Sniper () (\_ _ -> return  ((), Nothing))