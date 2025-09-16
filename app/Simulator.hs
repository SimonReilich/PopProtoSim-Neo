module Simulator where

run :: Int -> (IO ())
run x = putStrLn ("Hello World " ++ (show x))