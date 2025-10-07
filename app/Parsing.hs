module Parsing where

data Input = Input {p :: String, a :: [Int], x :: [Int], s :: String, c :: [Float]} | Message {str :: String}

help :: String
help =
  "usage: proto-sim [-h | --help] | (<protocol> [(-s | --sniper) <sniper>]) \n\n\
  \These are the available protocols with the required arguments and input: \n\n\
  \cut-protocol\n\
  \   cut <input>\n\
  \   input: x0\n\
  \   computes: x0\n\n\
  \modulo-protocol\n\
  \   mod <arg> <input>\n\
  \   arg: m\n\
  \   input: x0\n\
  \   computes: x0 mod m"

parse :: [String] -> Input
parse [] = Message "please have a look at the usage of the command: proto-sim --help"
parse (s : rest) =
  case s of
    "--help" -> Message help
    "-h" -> Message help
    "cut" ->
      if length rest == 1
        then Input "c" [] [read (head rest)] "n" []
        else if length rest == 4 && (rest !! 1 == "-s" || rest !! 1 == "--sniper") 
          then Input "c" [] [read (head rest)] "r" [read (last rest)]
          else Message "wrong number of arguments, please use --help for usage"
    "mod" ->
      if length rest == 2
        then Input "m" [read (head rest)] [read (rest !! 1)] "n" []
        else if length rest == 5 && (rest !! 2 == "-s" || rest !! 2 == "--sniper") 
          then Input "m" [read (head rest)] [read (rest !! 1)] "r" [read (last rest)]
          else Message "wrong number of arguments, please use --help for usage"
    _ -> Message "please have a look at the usage of the command: proto-sim --help"