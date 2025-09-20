module Config where

type Configuration a = [a]
type Input a = (Configuration a, a -> a -> (a, a), a -> Int)