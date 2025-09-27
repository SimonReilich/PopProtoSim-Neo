module Main where

import System.Random
import Protocols
import Protocols.Cut

main :: IO ()
main =
    putStrLn "Choose a protocol to simulate: " >>
    getChar >>=
    chooseProtocol

chooseProtocol :: Char -> IO ()
chooseProtocol p =
    case p of
        'c' -> do 
            putStrLn "x0 = 5"
            run (Protocols.Cut.get "5")
        _   -> putStrLn "no such protocol"

run :: (Eq a) => Protocols.Input a -> IO ()
run (c, d, s, o) =
    let
        helper states gen =
            if isStable (states, d, s, o)
                then putStrLn (printConfig states (-1) (-1) s) >> 
                    putStrLn "" >> 
                    putStrLn ("Output: " ++ show (o (head states)))
            else let
                (a1, a2, newStates, genNew) = step (states, d, s, o) gen
            in putStrLn (printConfig states (-1) (-1) s) >> 
                putStrLn (printConfig states a1 a2 s) >>
                putStrLn (printConfig newStates a1 a2 s) >>
                helper newStates genNew
    in helper c (mkStdGen (hash (c, d, s, o)))

step :: (Eq a) => Protocols.Input a -> StdGen -> (Int, Int, Protocols.Configuration a, StdGen)
step (states, delta, stringify, output) gen =
    let
        (a1, a2, genNew) = selectAgents (states, delta, stringify, output) gen
    in let
        (q1, q2) = delta (states!!a1) (states!!a2)
    in let
        newStates = repl a1 q1 (repl a2 q2 states)
    in (a1, a2, newStates, genNew)

isStable :: Protocols.Input a -> Bool
isStable (states, delta, _, output) =
    length (filter (\a -> output a == 5) states) == length states

selectAgents :: (Eq a) => Protocols.Input a -> StdGen -> (Int, Int, StdGen)
selectAgents (states, delta, _, _) g =
    let
        rnd :: StdGen -> (Int, StdGen)
        rnd = uniformR (0, length states - 1)
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
    in result g

printConfig :: Protocols.Configuration a -> Int -> Int -> (a -> String) -> String
printConfig [] _ _ _ = " |"
printConfig (s:states) a1 a2 stringify =
    if a1 == 0 || a2 == 0
        then "| *" ++ stringify s ++ "* " ++ printConfig states (a1 - 1) (a2 - 1) stringify
        else "|  " ++ stringify s ++ "  " ++ printConfig states (a1 - 1) (a2 - 1) stringify

hash :: Input a -> Int
hash (c, _, _, o) =
    let
        helper [] _ _ = 0
        helper (s:states) output n =
            output s ^ n + helper states output (n + 1)
    in helper c o (1 :: Integer)

repl :: Int -> a -> [a] -> [a]
repl _ _ [] =
    []
repl i e (x:xs) =
    if i <= 0
        then e:xs
        else x:repl (i - 1) e xs