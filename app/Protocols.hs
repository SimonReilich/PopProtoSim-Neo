module Protocols where

type Configuration a = [a]

type Input a = (Configuration a, a -> a -> (a, a), a -> String, a -> Int)