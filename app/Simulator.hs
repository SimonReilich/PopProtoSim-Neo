module Simulator where

import Util
import System.Random

run :: (Eq a) => Util.Input a -> IO ()
run (states, delta, stringify, output) = 
    let 
        helper (states, delta, stringify, output) gen = 
            if isStable (states, delta, stringify, output)
                then putStrLn "" >> putStrLn ("Output: " ++ (show (output (head states))))
            else let
                (a1, a2, genNew) = selectAgents (states, delta, stringify, output) gen
            in let
                (q1, q2) = delta (states!!a1) (states!!a2)
            in let 
                newConfig = Util.repl a1 q1 (Util.repl a2 q2 states)
            in putStrLn (printConfig newConfig a1 a2 stringify) >> helper (newConfig, delta, stringify, output) genNew
    in helper (states, delta, stringify, output) (mkStdGen (Util.hash (states, delta, stringify, output)))

isStable :: Util.Input a -> Bool
isStable (states, delta, stringify, output) =
    length (filter (\a -> output a == 5) states) == length states

selectAgents :: (Eq a) => Util.Input a -> StdGen -> (Int, Int, StdGen)
selectAgents (states, delta, stringify, output) gen = 
    let 
        rnd :: StdGen -> (Int, StdGen)
        rnd = uniformR (0, (length states) - 1)
    in let 
        result gen = 
            let 
                (a1, genNew1) = rnd gen
            in let
                (a2, genNew2) = rnd genNew1
            in let
                (q1, q2) = delta (states!!a1) (states!!a2)
            in if a1 == a2 || (q1 == states!!a1 && q2 == states!!a2) || (q1 == states!!a2 && q2 == states!!a1)
                then result genNew2
                else (a1, a2, genNew2)
    in result gen

printConfig :: Util.Configuration a -> Int -> Int -> (a -> String) -> String
printConfig [] a1 a2 stringify = " |"
printConfig (s:states) a1 a2 stringify =
    if a1 == 0 || a2 == 0
        then "| *" ++ (stringify s) ++ "* " ++ (printConfig states (a1 - 1) (a2 - 1) stringify)
        else "|  " ++ (stringify s) ++ "  " ++ (printConfig states (a1 - 1) (a2 - 1) stringify)
