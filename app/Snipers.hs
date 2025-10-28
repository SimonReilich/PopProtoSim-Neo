module Snipers where

import Data.List
import Protocols
import System.IO
import System.Random

type Sniper a s = (s, Configuration a -> s -> IO (s, Maybe Int))

randomSniper :: Int -> Float -> Sniper a StdGen
randomSniper seed chance =
  let snipe config gen =
        if length (Data.List.filter fst config) >= 2
          then
            let (res, genNew) = randomR (0.0, 1.0) gen
             in if res < chance
                  then
                    let helper gen =
                          let (agent, genNew) = randomR (0, length config - 1) gen
                           in if (case config !! agent of (b, _) -> b)
                                then return (genNew, Just agent)
                                else helper genNew
                     in helper genNew
                  else return (genNew, Nothing)
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

specialSniper :: Int -> Sniper (Int, [Int], Int) ()
specialSniper m =
  let snipe config _ = case length $ filter (not . fst) config of
        0 -> return ((), findIndex (\(_, (l, v, _)) -> l == 2 && v !! l == 4) config )
        1 -> return ((), findIndex (\(_, (l, v, _)) -> l == 5 && v !! l == 3) config )
        2 -> return ((), findIndex (\(_, (l, v, _)) -> l == 8 && v !! l == 2) config )
        _ -> return ((), Nothing)
  in ((), snipe)


noSniper :: Sniper a ()
noSniper =
  ((), \_ _ -> return ((), Nothing))