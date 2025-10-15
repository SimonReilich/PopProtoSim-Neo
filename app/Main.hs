{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad
import Data.List
import Data.List.Duplicate
import Data.Maybe
import Protocols
import Snipers
import Protocols.Cut hiding (input, stringify, delta, output)
import Protocols.Modulo hiding (input, stringify, delta, output)
import Protocols.Combined hiding (output)
import System.Environment
import System.Random
import System.Console.Docopt
import Text.Colour
import Util
import Data.Text hiding (length, head, replace, replicate, find, any, null)
import qualified Snipers as Sniper

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` longOption "help") $ do
    readFile "USAGE.txt" >>= putStrLn

  when (args `isPresent` command "cut") $ do
    x0 <- args `getArgOrExit` argument "x0"
    t <- args `getArgOrExit` argument "t"
    seed <- getSeed args
    delay <- getDelay args
    let proto = Protocols.Cut.get (read t)
      in case getArgWithDefault args "" (longOption "sniper") of
        "" -> simulate proto [read x0] (if args `isPresent` longOption "manual" then Sniper.manualSniper else Sniper.noSniper) seed delay (not (args `isPresent` longOption "noCheck"))
        rt -> simulate proto [read x0] (Snipers.randomSniper seed (read rt)) seed delay (not (args `isPresent` longOption "noCheck"))

  when (args `isPresent` command "mod") $ do
    x0 <- args `getArgOrExit` argument "x0"
    m <- args `getArgOrExit` argument "m"
    seed <- getSeed args
    delay <- getDelay args
    let proto = Protocols.Modulo.get (read m)
      in case getArgWithDefault args "" (longOption "sniper") of
        "" -> simulate proto [read x0] (if args `isPresent` longOption "manual" then Sniper.manualSniper else Sniper.noSniper) seed delay (not (args `isPresent` longOption "noCheck"))
        rt -> simulate proto [read x0] (Snipers.randomSniper seed (read rt)) seed delay (not (args `isPresent` longOption "noCheck"))

  when (args `isPresent` command "cmb") $ do
    x0 <- args `getArgOrExit` argument "x0"
    m <- args `getArgOrExit` argument "m"
    t <- args `getArgOrExit` argument "t"
    seed <- getSeed args
    delay <- getDelay args
    let proto = Protocols.Combined.get (read m) (read t)
      in case getArgWithDefault args "" (longOption "sniper") of
        "" -> simulate proto [read x0] (if args `isPresent` longOption "manual" then Sniper.manualSniper else Sniper.noSniper) seed delay (not (args `isPresent` longOption "noCheck"))
        rt -> simulate proto [read x0] (Snipers.randomSniper seed (read rt)) seed delay (not (args `isPresent` longOption "noCheck"))

getSeed :: Arguments -> IO Int
getSeed args =
  case getArgWithDefault args "" (longOption "random") of
    "" -> return 0
    seed -> return (read seed)

getDelay :: Arguments -> IO Int
getDelay args =
  case getArgWithDefault args "" (longOption "delay") of
    "" -> return 0
    delay -> return (round ((read delay :: Double) * 1000000))

simulate :: (Eq a) => Protocol a -> [Int] -> Sniper a s -> Int -> Int -> Bool -> IO ()
simulate (m, d, s, o) x sn seed delay doCheck =
  let helper states sniper gen =
        if isStable doCheck states (m, d, s, o)
          then
            let Just output = getOutput states (m, d, s, o)
             in putStrLn "" >> putChunksUtf8With With24BitColours [chunk (pack "Output: "), output]
          else do
            (newStates, newSniper, newGen) <- step states (m, d, s, o) sniper gen delay doCheck
            helper newStates newSniper newGen
   in putStrLn "" >> helper (Protocols.getInitial (m, d, s, o) x) sn (mkStdGen seed)

step :: (Eq a) => Configuration a -> Protocol a -> Sniper a s -> StdGen -> Int -> Bool -> IO (Configuration a, Sniper a s, StdGen)
step states (mapping, delta, stringify, output) (sniperState, sniping) gen delay doCheck = do
  let (a1, a2, newGen) = selectAgents states (mapping, delta, stringify, output) gen
   in let (q1, q2) = Protocols.deltaWrapper delta (states !! a1) (states !! a2)
       in let newStates = replace a1 q1 (replace a2 q2 states)
           in do 
            printConfig states a1 a2 stringify delay
            printConfig newStates (-1) (-1) stringify delay
            if not (isStable doCheck newStates (mapping, delta, stringify, output))
              then do
                (newSniperState, killed) <- sniping newStates sniperState
                case killed of
                  Just i -> return (replace i (False, let (_, s) = newStates !! i in s) newStates, (newSniperState, sniping), newGen)
                  Nothing -> return (newStates, (newSniperState, sniping), newGen)
              else return (newStates, (sniperState, sniping), newGen)

isStable :: (Eq a) => Bool -> Configuration a -> Protocol a -> Bool
isStable doCheck c (m, d, s, o) =
  let getAllConf states (mapping, delta, stringify, output) = 
        case states of
          [] -> []
          x : xs ->
            Data.Maybe.mapMaybe
              ( \i ->
                  let (xn, q) = delta x (xs !! i)
                   in if (xn == x && q == xs !! i) || (q == x && xn == xs !! i)
                        then Nothing
                        else Just (xn : replace i q xs)
              )
              (deleteDups (Data.Maybe.mapMaybe (`elemIndex` xs) xs))
              ++ Data.List.map (x :) (getAllConf xs (mapping, delta, stringify, output))
   in let helperCheck [] _ _ =
            True
          helperCheck (current : queue) found output =
            let successors = getAllConf current (m, d, s, o)
             in case find (any (\state -> output /= o state)) successors of
                  Just _ -> False
                  Nothing -> helperCheck (queue ++ Data.List.filter (\state -> notElem state found && notElem state queue) successors) (current : found) output
          helperNoCheck [] _ =
            True
          helperNoCheck (first : states) delta =
            (not (Data.List.any (\s -> (
                let (q1, q2) = delta first s
                in not ((q1 == first && q2 == s) || (q1 == s && q2 == first)))) states) && helperNoCheck states delta)
        in case getOutput c (m, d, s, o) of
            Just r -> if doCheck
              then helperCheck [Data.Maybe.mapMaybe (\(b, state) -> if b then Just state else Nothing) c] [] r
              else helperNoCheck (Data.Maybe.mapMaybe (\(b, state) -> if b then Just state else Nothing) c) d
            Nothing -> False

selectAgents :: (Eq a) => Configuration a -> Protocol a -> StdGen -> (Int, Int, StdGen)
selectAgents states (_, delta, _, _) g =
  let result gen =
        let (a1, newGen1) = randomR (0, length states - 1) gen
         in let (a2, newGen2) = randomR (0, length states - 1) newGen1
             in let ((b1, q1), (b2, q2)) = Protocols.deltaWrapper delta (states !! a1) (states !! a2)
                 in if b1 && b2
                      then
                        if a1 == a2 || ((b1, q1) == states !! a1 && (b2, q2) == states !! a2) || ((b1, q1) == states !! a2 && (b2, q2) == states !! a1)
                          then result newGen2
                          else (a1, a2, newGen2)
                      else result newGen2
   in result g

getOutput :: Configuration a -> Protocol a -> Maybe Chunk
getOutput c (_, _, _, o) =
  let outputs = Data.List.map head (Data.List.group (Data.Maybe.mapMaybe (\(b, s) -> if b then Just (o s) else Nothing) c))
   in case outputs of
        [r] -> Just r
        _ -> Nothing