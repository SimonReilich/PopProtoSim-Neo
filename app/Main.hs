{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad
import Data.List
import Data.List.Duplicate
import Data.Maybe
import Protocols
import Snipers
import Protocols.Cut
import Protocols.Modulo
import Protocols.Combined
import System.Environment
import System.Random
import System.Console.Docopt
import Util

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` command "cut") $ do
    x0 <- args `getArgOrExit` argument "x0"
    let proto = Protocols.Cut.get 
      in simulate proto [read x0] Snipers.noSniper

  when (args `isPresent` command "mod") $ do
    x0 <- args `getArgOrExit` argument "x0"
    m <- args `getArgOrExit` argument "m"
    let proto = Protocols.Modulo.get (read m)
      in simulate proto [read x0] Snipers.noSniper

  when (args `isPresent` command "cmb") $ do
    x0 <- args `getArgOrExit` argument "x0"
    m <- args `getArgOrExit` argument "m"
    let proto = Protocols.Combined.get (read m)
      in simulate proto [read x0] Snipers.noSniper

simulate :: (Eq a) => Protocol a -> [Int] -> Sniper a s -> IO ()
simulate (m, d, s, o) x sn =
  let helper states sniper gen =
        if isStable states (m, d, s, o)
          then
            let Just output = getOutput states (m, d, s, o)
             in putStrLn (printConfig states (-1) (-1) s)
                  >> putStrLn ""
                  >> putStrLn ("Output: " ++ output)
          else
            let (a1, a2, newStates, newSniper, newGen) = step states (m, d, s, o) sniper gen
             in putStrLn (printConfig states (-1) (-1) s)
                  >> putStrLn (printConfig states a1 a2 s)
                  >> putStrLn (printConfig newStates a1 a2 s)
                  >> helper newStates newSniper newGen
   in helper (Protocols.getInitial (m, d, s, o) x) sn (mkStdGen 0)

step :: (Eq a) => Configuration a -> Protocol a -> Sniper a s -> StdGen -> (Int, Int, Configuration a, Sniper a s, StdGen)
step states (mapping, delta, stringify, output) (sniperState, sniping) gen =
  let (a1, a2, newGen) = selectAgents states (mapping, delta, stringify, output) gen
   in let (q1, q2) = Protocols.deltaWrapper delta (states !! a1) (states !! a2)
       in let newStates = replace a1 q1 (replace a2 q2 states)
           in let (newSniperState, killed) = sniping newStates sniperState
               in case killed of
                    Just i -> (a1, a2, replace i (False, let (_, s) = newStates !! i in s) newStates, (newSniperState, sniping), newGen)
                    Nothing -> (a1, a2, newStates, (newSniperState, sniping), newGen)

isStable :: (Eq a) => Configuration a -> Protocol a -> Bool
isStable c (m, d, s, o) =
  let getAllConf states (mapping, delta, stringify, output) =
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
              ++ Data.List.map (x :) (getAllConf xs (mapping, delta, stringify, output))
   in let helper [] _ _ =
            True
          helper (current : queue) found output =
            let successors = getAllConf current (m, d, s, o)
             in case find (any (\state -> output /= o state)) successors of
                  Just _ -> False
                  Nothing -> helper (queue ++ Data.List.filter (\state -> notElem state found && notElem state queue) successors) (current : found) output
       in case getOutput c (m, d, s, o) of
            Just r -> helper [Data.Maybe.mapMaybe (\(b, s) -> if b then Just s else Nothing) c] [] r
            Nothing -> False

selectAgents :: (Eq a) => Configuration a -> Protocol a -> StdGen -> (Int, Int, StdGen)
selectAgents states (_, delta, _, _) g =
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

getOutput :: Configuration a -> Protocol a -> Maybe String
getOutput c (_, _, _, o) =
  let outputs = Data.List.map head (Data.List.group (Data.Maybe.mapMaybe (\(b, s) -> if b then Just (o s) else Nothing) c))
   in case outputs of
        [r] -> Just r
        _ -> Nothing