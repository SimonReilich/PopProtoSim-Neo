module Simulator where

import Config

run :: Config.Input a -> Config.Configuration a
run (states, delta, output) = 
    if stable states
        then states
    else let
        a1 = 0
    in let
        a2 = 1
    in 

stable :: Config.Configuration a -> Bool
stable