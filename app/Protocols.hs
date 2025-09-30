module Protocols where

type Configuration a = [a]

type Protocol a = (Configuration a, a -> a -> (a, a), a -> String, a -> Int)