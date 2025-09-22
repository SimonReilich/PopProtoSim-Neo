module Main where

import Simulator
import Protocols.Cut

main :: IO ()
main =
    putStrLn "Choose a protocol to simulate: " >>
    getChar >>=
    chooseProtocol

chooseProtocol :: Char -> IO ()
chooseProtocol p =
    case p of
        'c' -> putStrLn "" >> Simulator.run (Protocols.Cut.get [] [5])
        _   -> putStrLn "no such protocol"