module Main where

import Data.List
import Data.List.Duplicate
import Data.Maybe
import Parsing
import Protocols
import Protocols.Cut
import Protocols.Modulo
import Protocols.Combined
import System.Environment
import System.Random
import Util

main :: IO ()
main = do
  args <- getArgs
  run (parse args)

run :: Parsing.Input -> IO ()
run input =
  case input of
    Message str -> putStrLn str
    Input p a x s c ->
      case (p, s) of
        ("c", "n") -> do
          putStrLn ("\nx0 = " ++ show (head x) ++ "\n")
          simulate (Protocols.Cut.get (head x) Protocols.noSniper)
        ("c", "r") -> do
          putStrLn ("\nx0 = " ++ show (head x) ++ "\n")
          simulate (Protocols.Cut.get (head x) (Protocols.randomSniper (head c)))
        ("m", "n") -> do
          putStrLn ("\na0 = " ++ show (head a) ++ "; x0 = " ++ show (head x) ++ "\n")
          simulate (Protocols.Modulo.get (head a) (head x) Protocols.noSniper)
        ("m", "r") -> do
          putStrLn ("\na0 = " ++ show (head a) ++ "; x0 = " ++ show (head x) ++ "\n")
          simulate (Protocols.Modulo.get (head a) (head x) (Protocols.randomSniper (head c)))
        ("b", "n") -> do
          putStrLn ("\na0 = " ++ show (head a) ++ "; x0 = " ++ show (head x) ++ "\n")
          simulate (Protocols.Combined.get (head a) (head x) Protocols.noSniper)
        ("b", "r") -> do
          putStrLn ("\na0 = " ++ show (head a) ++ "; x0 = " ++ show (head x) ++ "\n")
          simulate (Protocols.Combined.get (head a) (head x) (Protocols.randomSniper (head c)))
        _ -> putStrLn "no such protocol"

simulate :: (Eq a) => Protocols.Protocol a b -> IO ()
simulate (c, d, s, o, sn) =
  let helper states sniper gen =
        if isStable (states, d, s, o, sniper)
          then
            let Just output = getOutput (states, d, s, o, sniper)
             in putStrLn (printConfig states (-1) (-1) s)
                  >> putStrLn ""
                  >> putStrLn ("Output: " ++ output)
          else
            let (a1, a2, newStates, newSniper, newGen) = step (states, d, s, o, sniper) gen
             in putStrLn (printConfig states (-1) (-1) s)
                  >> putStrLn (printConfig states a1 a2 s)
                  >> putStrLn (printConfig newStates a1 a2 s)
                  >> helper newStates newSniper newGen
   in helper c sn (mkStdGen 0)

step :: (Eq a) => Protocols.Protocol a b -> StdGen -> (Int, Int, Protocols.Configuration a, Protocols.Sniper a b, StdGen)
step (states, delta, stringify, output, (sniperState, sniping)) gen =
  let (a1, a2, newGen) = selectAgents (states, delta, stringify, output, (sniperState, sniping)) gen
   in let (q1, q2) = Protocols.deltaWrapper delta (states !! a1) (states !! a2)
       in let newStates = replace a1 q1 (replace a2 q2 states)
           in let (newSniperState, killed) = sniping newStates sniperState
               in case killed of
                    Just i -> (a1, a2, replace i (False, let (_, s) = newStates !! i in s) newStates, (newSniperState, sniping), newGen)
                    Nothing -> (a1, a2, newStates, (newSniperState, sniping), newGen)

isStable :: (Eq a) => Protocols.Protocol a b -> Bool
isStable (c, d, s, o, sn) =
  let getAllConf (states, delta, stringify, output, sniper) =
        case states of
          [] -> []
          x : xs ->
            Data.Maybe.mapMaybe
              ( \i ->
                  let (xn, q) = delta x (xs !! i)
                   in if (xn == x && q == xs !! i) || (q == x && xn == xs !! i)
                        then Nothing
                        else Just (xn : replace i q xs)
              )
              (deleteDups (Data.Maybe.mapMaybe (`elemIndex` xs) xs))
              ++ Data.List.map (x :) (getAllConf (xs, delta, stringify, output, sniper))
   in let helper [] _ _ =
            True
          helper (current : queue) found output =
            let successors = getAllConf (current, d, s, o, sn)
             in case find (any (\state -> output /= o state)) successors of
                  Just _ -> False
                  Nothing -> helper (queue ++ Data.List.filter (\state -> notElem state found && notElem state queue) successors) (current : found) output
       in case getOutput (c, d, s, o, sn) of
            Just r -> helper [Data.Maybe.mapMaybe (\(b, s) -> if b then Just s else Nothing) c] [] r
            Nothing -> False

selectAgents :: (Eq a) => Protocols.Protocol a b -> StdGen -> (Int, Int, StdGen)
selectAgents (states, delta, _, _, _) g =
  let result gen =
        let (a1, newGen1) = randomR (0, length states - 1) gen
         in let (a2, newGen2) = randomR (0, length states - 1) newGen1
             in let ((b1, q1), (b2, q2)) = Protocols.deltaWrapper delta (states !! a1) (states !! a2)
                 in if b1 && b2
                      then
                        if a1 == a2 || ((b1, q1) == states !! a1 && (b2, q2) == states !! a2) || ((b1, q1) == states !! a2 && (b2, q2) == states !! a1)
                          then result newGen2
                          else (a1, a2, newGen2)
                      else result newGen2
   in result g

getOutput :: Protocols.Protocol a b -> Maybe String
getOutput (c, _, _, o, _) =
  let outputs = Data.List.map head (Data.List.group (Data.Maybe.mapMaybe (\(b, s) -> if b then Just (o s) else Nothing) c))
   in case outputs of
        [r] -> Just r
        _ -> Nothing