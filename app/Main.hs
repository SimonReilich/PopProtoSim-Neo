module Main where

import Data.List
import Data.List.Duplicate
import Data.Maybe
import Protocols
import Protocols.Cut
import System.Environment
import System.Random
import Util

main :: IO ()
main = do
  args <- getArgs
  chooseProtocol args

chooseProtocol :: [String] -> IO ()
chooseProtocol p =
  case head p of
    "c" -> do
      putStrLn ("\nx0 = " ++ (p !! 1) ++ "\n")
      run (Protocols.Cut.get (p !! 1))
    _ -> putStrLn "no such protocol"

run :: (Eq a) => Protocols.Input a -> IO ()
run (c, d, s, o) =
  let helper states gen =
        if isStable (states, d, s, o)
          then
            putStrLn (printConfig states (-1) (-1) s)
              >> putStrLn ""
              >> putStrLn ("Output: " ++ show (o (head states)))
          else
            let (a1, a2, newStates, genNew) = step (states, d, s, o) gen
             in putStrLn (printConfig states (-1) (-1) s)
                  >> putStrLn (printConfig states a1 a2 s)
                  >> putStrLn (printConfig newStates a1 a2 s)
                  >> helper newStates genNew
   in helper c (mkStdGen (hash (c, d, s, o)))

step :: (Eq a) => Protocols.Input a -> StdGen -> (Int, Int, Protocols.Configuration a, StdGen)
step (states, delta, stringify, output) gen =
  let (a1, a2, genNew) = selectAgents (states, delta, stringify, output) gen
   in let (q1, q2) = delta (states !! a1) (states !! a2)
       in let newStates = replace a1 q1 (replace a2 q2 states)
           in (a1, a2, newStates, genNew)

isStable :: (Eq a) => Protocols.Input a -> Bool
isStable (c, d, s, o) =
  let getAllConf (states, delta, stringify, output) =
        case states of
          [] -> []
          x : xs ->
            Data.Maybe.mapMaybe
              ( \i ->
                  let (xn, q) = delta x (tail states !! i)
                   in if (xn == x && q == tail states !! i) || (q == x && xn == tail states !! i)
                        then Nothing
                        else Just (xn : replace i q (tail states))
              )
              (deleteDups (Data.Maybe.mapMaybe (`elemIndex` tail states) (tail states)))
              ++ Data.List.map (x :) (getAllConf (xs, delta, stringify, output))
   in let helper [] _ =
            True
          helper (current : queue) found =
            let successors = getAllConf (current, d, s, o)
             in case find (any (\state -> o (head c) /= o state)) successors of
                  Just _ -> False
                  Nothing -> helper (queue ++ Data.List.filter (\state -> notElem state found && notElem state queue) successors) (current : found)
       in helper [c] []

selectAgents :: (Eq a) => Protocols.Input a -> StdGen -> (Int, Int, StdGen)
selectAgents (states, delta, _, _) g =
  let rnd :: StdGen -> (Int, StdGen)
      rnd = uniformR (0, length states - 1)
   in let result gen =
            let (a1, genNew1) = rnd gen
             in let (a2, genNew2) = rnd genNew1
                 in let (q1, q2) = delta (states !! a1) (states !! a2)
                     in if a1 == a2 || (q1 == states !! a1 && q2 == states !! a2) || (q1 == states !! a2 && q2 == states !! a1)
                          then result genNew2
                          else (a1, a2, genNew2)
       in result g
