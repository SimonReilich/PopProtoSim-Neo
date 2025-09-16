module Main where

import Simulator
import Protocols.Cut
import Protocols.Modulo

main :: IO ()
main =
    putStrLn "Choose a protocol to simulate: " >>
    getChar >>=
    chooseProtocol

chooseProtocol :: Char -> IO ()
chooseProtocol p =
    case p of
        'c' -> Simulator.run Protocols.Cut.get
        'm' -> Simulator.run Protocols.Modulo.get
        _   -> putStrLn "no such protocol"