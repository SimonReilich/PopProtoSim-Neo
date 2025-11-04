{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

module Snipers where

import Data.List
import Protocols
import System.IO
import System.Random
import Control.Monad

data Sniper a s = Sniper s (Configuration a -> s -> IO (s, Maybe Int))

randomSniper :: (Eq a, Show a) => Int -> Float -> Sniper a StdGen
randomSniper seed chance =
  let snipe :: (Eq a, Show a) => Configuration a -> StdGen -> IO (StdGen, Maybe Int)
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

manualSniper :: (Eq a, Show a) => Sniper a Bool
manualSniper =
  let snipe :: (Eq a, Show a) => Configuration a -> Bool -> IO (Bool, Maybe Int)
      snipe config b =
        if null config
          then return (b, Nothing)
          else do
            if b then (do
              putStr "\nAgent to be sniped: "
              hFlush stdout
              (do putStrLn ""
                  agent <- readLn :: IO Int
                  if agent > 0 && agent <= length config 
                    then return (True, Just (agent - 1)) 
                    else if agent == -1
                      then return (False, Nothing)
                      else return (True, Nothing)))
              else return (False, Nothing)
   in Sniper True snipe

noSniper :: (Eq a, Show a) => Sniper a ()
noSniper =
  Sniper () (\_ _ -> return ((), Nothing))