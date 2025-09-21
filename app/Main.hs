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
        'c' -> print (Simulator.run (Protocols.Cut.get [3]))
        _   -> putStrLn "no such protocol"