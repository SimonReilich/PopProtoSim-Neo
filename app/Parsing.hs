module Parsing where

import Data.List
import Data.List.Split
import Data.Maybe

data Input = Input { p :: String, a :: [Int], x :: [Int] } | Message { str :: String }

help :: String
help = "usage: proto-sim [-h | --help] <protocol> [<args>] <input> \n\n\

\These are the available protocols with the required arguments and input: \n\n\

\cut-protocol\n\
\   cut\n\
\   required args: none\n\
\   required inputs: x0\n\n\

\modulo-protocol\n\
\   mod\n\
\   required args: m\n\
\   required inputs: x0"

parse :: [String] -> Input
parse (s : str) =
  case s of
    "--help" -> Message help
    "-h" -> Message help
    "cut" -> if length str == 1 
        then Input "c" [] [read (str !! 0)]
        else Message "\nwrong number of arguments, please use --help for ussage"
    "mod" -> if length str == 2 
        then Input "m" [read (str !! 0)] [read (str !! 1)]
        else Message "\nwrong number of arguments, please use --help for ussage"