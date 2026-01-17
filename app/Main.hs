{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Monad
import Data.List
import Data.List.Duplicate
import Data.Maybe
import Data.Text hiding (any, concat, concatMap, filter, find, head, length, map, null, replace, replicate, show, zipWith)
import Protocols
import Protocols.Combined hiding (delta, output, stringify)
import Protocols.Cut hiding (delta, input, output, stringify)
import Protocols.Modulo hiding (delta, input, output, stringify)
import Protocols.Tuple hiding (delta, input, output, stringify)
import Snipers
import System.Console.Docopt
import System.Environment
import System.Random
import Text.Colour
import Util

import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))

patterns :: Docopt
patterns =
  [docopt|
proto-sim version 0.1.0.0

Usage:
  proto-sim cut [-smdrn] <x0> <t>
  proto-sim mod [-smdrn] <x0> <m>
  proto-sim cmb [-smdrn] <x0> <m> <t>
  proto-sim p [-smdrn] (<xi> <mi> <ti>)...
  proto-sim cutstat <x0Max> <tMin> <tMax> -srpn
  proto-sim modstat <x0Max> <mMin> <mMax> -srpn
  proto-sim cmbstat <x0Max> <mMin> <mMax> <tMin> <tMax> -srpn
  proto-sim pstat <x0Max> <mMin> <mMax> <tMin> <tMax> -srpn
  proto-sim -h

Options:
  -s=<rt>, --sniper=<rt>    Enables the sniper with rate <rt>.
  -m, --manual              Enables the manual sniper, not compatible with -d.
  -d=<dl>, --delay=<dl>     Prints the execution steps with delay <dl> [default: 0.0].
  -r=<sd>, --random=<sd>    Custom seed for random number generation.
  -p=<fl>, --path=<fl>      Path of the output file [default: out.dat].
  -n, --nocheck             Disable the check for convergence, necessary for larger inputs.
  -h, --help                Show this screen.
|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` longOption "help") $ do
    putStrLn "\n\
      \proto-sim version 0.1.0.0\n\n\

      \Usage:\n\
      \  proto-sim cut [-smdrn] <x0> <t>\n\
      \  proto-sim mod [-smdrn] <x0> <m>\n\
      \  proto-sim cmb [-smdrn] <x0> <m> <t>\n\
      \  proto-sim p [-smdrn] (<xi> <mi> <ti>)...\n\
      \  proto-sim cutstat <x0Max> <tMin> <tMax> -srpn\n\
      \  proto-sim modstat <x0Max> <mMin> <mMax> -srpn\n\
      \  proto-sim cmbstat <x0Max> <mMin> <mMax> <tMin> <tMax> -srpn\n\
      \  proto-sim pstat <x0Max> <mMin> <mMax> <tMin> <tMax> -srpn\n\
      \  proto-sim -h\n\n\

      \Options:\n\
      \  -s=<rt>, --sniper=<rt>    Enables the sniper with rate <rt>.\n\
      \  -m, --manual              Enables the manual sniper, not compatible with -d.\n\
      \  -d=<dl>, --delay=<dl>     Prints the execution steps with delay <dl> [default: 0.0].\n\
      \  -r=<sd>, --random=<sd>    Custom seed for random number generation.\n\
      \  -p=<fl>, --path=<fl>      Path of the output file [default: out.dat].\n\
      \  -n, --nocheck             Disable the check for convergence, necessary for larger inputs.\n\
      \  -h, --help                Show this screen."

  when (args `isPresent` command "cut") $ do
    x0 <- args `getArgOrExit` argument "x0"
    t <- args `getArgOrExit` argument "t"
    seed <- getSeed args
    delay <- getDelay args
    let proto = Protocols.Cut.get (read t)
     in case getArgWithDefault args "" (longOption "sniper") of
          "" ->
            if args `isPresent` longOption "manual"
              then do
                result <- simulate proto [read x0] (Snipers.manualSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                end result [read x0] (Protocols.Cut.test (read t))
              else do
                result <- simulate proto [read x0] (Snipers.noSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                end result [read x0] (Protocols.Cut.test (read t))
          rt -> do
            result <- simulate proto [read x0] (Snipers.randomSniper seed (read rt)) seed delay (not (args `isPresent` longOption "nocheck")) True
            end result [read x0] (Protocols.Cut.test (read t))

  when (args `isPresent` command "mod") $ do
    x0 <- args `getArgOrExit` argument "x0"
    m <- args `getArgOrExit` argument "m"
    seed <- getSeed args
    delay <- getDelay args
    let proto = Protocols.Modulo.get (read m)
     in case getArgWithDefault args "" (longOption "sniper") of
          "" ->
            if args `isPresent` longOption "manual"
              then do
                result <- simulate proto [read x0] (Snipers.manualSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                end result [read x0] (Protocols.Modulo.test (read m))
              else do
                result <- simulate proto [read x0] (Snipers.noSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                end result [read x0] (Protocols.Modulo.test (read m))
          rt -> do
            result <- simulate proto [read x0] (Snipers.randomSniper seed (read rt)) seed delay (not (args `isPresent` longOption "nocheck")) True
            end result [read x0] (Protocols.Modulo.test (read m))

  when (args `isPresent` command "cmb") $ do
    x0 <- args `getArgOrExit` argument "x0"
    m <- args `getArgOrExit` argument "m"
    t <- args `getArgOrExit` argument "t"
    seed <- getSeed args
    delay <- getDelay args
    let proto = Protocols.Combined.get (read m) (read t)
     in case getArgWithDefault args "" (longOption "sniper") of
          "" ->
            if args `isPresent` longOption "manual"
              then do
                result <- simulate proto [read x0] (Snipers.manualSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                end result [read x0] (Protocols.Combined.test (read m) (read t))
              else do
                result <- simulate proto [read x0] (Snipers.noSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                end result [read x0] (Protocols.Combined.test (read m) (read t))
          rt -> do
            result <- simulate proto [read x0] (Snipers.randomSniper seed (read rt)) seed delay (not (args `isPresent` longOption "nocheck")) True
            end result [read x0] (Protocols.Combined.test (read m) (read t))

  when (args `isPresent` command "p" && args `getArgCount` argument "xi" <= 5 && args `getArgCount` argument "xi" >= 2) $
    ( do
        seed <- getSeed args
        delay <- getDelay args
        let x = args `getAllArgs` argument "xi"
         in let m = args `getAllArgs` argument "mi"
             in let t = args `getAllArgs` argument "ti"
                 in case args `getArgCount` argument "xi" of
                      2 ->
                        let proto = Protocols.Tuple.get (Protocols.Combined.get (read (m !! 0)) (read (t !! 0))) (Protocols.Combined.get (read (m !! 1)) (read (t !! 1)))
                         in case getArgWithDefault args "" (longOption "sniper") of
                              "" ->
                                if args `isPresent` longOption "manual"
                                  then do
                                    result <- simulate proto (map read x) (Snipers.manualSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                                    end result (map read x) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 0)) (read (t !! 0))) (Protocols.Combined.test (read (m !! 1)) (read (t !! 1))))
                                  else do
                                    result <- simulate proto (map read x) (Snipers.noSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                                    end result (map read x) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 0)) (read (t !! 0))) (Protocols.Combined.test (read (m !! 1)) (read (t !! 1))))
                              rt -> do
                                result <- simulate proto (map read x) (Snipers.randomSniper seed (read rt)) seed delay (not (args `isPresent` longOption "nocheck")) True
                                end result (map read x) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 0)) (read (t !! 0))) (Protocols.Combined.test (read (m !! 1)) (read (t !! 1))))
                      3 ->
                        let proto = Protocols.Tuple.get (Protocols.Combined.get (read (m !! 0)) (read (t !! 0))) (Protocols.Tuple.get (Protocols.Combined.get (read (m !! 1)) (read (t !! 1))) (Protocols.Combined.get (read (m !! 2)) (read (t !! 2))))
                         in case getArgWithDefault args "" (longOption "sniper") of
                              "" ->
                                if args `isPresent` longOption "manual"
                                  then do
                                    result <- simulate proto (map read x) (Snipers.manualSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                                    end result (map read x) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 0)) (read (t !! 0))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 1)) (read (t !! 1))) (Protocols.Combined.test (read (m !! 2)) (read (t !! 2)))))
                                  else do
                                    result <- simulate proto (map read x) (Snipers.noSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                                    end result (map read x) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 0)) (read (t !! 0))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 1)) (read (t !! 1))) (Protocols.Combined.test (read (m !! 2)) (read (t !! 2)))))
                              rt -> do
                                result <- simulate proto (map read x) (Snipers.randomSniper seed (read rt)) seed delay (not (args `isPresent` longOption "nocheck")) True
                                end result (map read x) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 0)) (read (t !! 0))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 1)) (read (t !! 1))) (Protocols.Combined.test (read (m !! 2)) (read (t !! 2)))))
                      4 ->
                        let proto = Protocols.Tuple.get (Protocols.Combined.get (read (m !! 0)) (read (t !! 0))) (Protocols.Tuple.get (Protocols.Combined.get (read (m !! 1)) (read (t !! 1))) (Protocols.Tuple.get (Protocols.Combined.get (read (m !! 2)) (read (t !! 2))) (Protocols.Combined.get (read (m !! 3)) (read (t !! 3)))))
                         in case getArgWithDefault args "" (longOption "sniper") of
                              "" ->
                                if args `isPresent` longOption "manual"
                                  then do
                                    result <- simulate proto (map read x) (Snipers.manualSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                                    end result (map read x) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 0)) (read (t !! 0))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 1)) (read (t !! 1))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 2)) (read (t !! 2))) (Protocols.Combined.test (read (m !! 3)) (read (t !! 3))))))
                                  else do
                                    result <- simulate proto (map read x) (Snipers.noSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                                    end result (map read x) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 0)) (read (t !! 0))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 1)) (read (t !! 1))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 2)) (read (t !! 2))) (Protocols.Combined.test (read (m !! 3)) (read (t !! 3))))))
                              rt -> do
                                result <- simulate proto (map read x) (Snipers.randomSniper seed (read rt)) seed delay (not (args `isPresent` longOption "nocheck")) True
                                end result (map read x) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 0)) (read (t !! 0))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 1)) (read (t !! 1))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 2)) (read (t !! 2))) (Protocols.Combined.test (read (m !! 3)) (read (t !! 3))))))
                      5 ->
                        let proto = Protocols.Tuple.get (Protocols.Combined.get (read (m !! 0)) (read (t !! 0))) (Protocols.Tuple.get (Protocols.Combined.get (read (m !! 1)) (read (t !! 1))) (Protocols.Tuple.get (Protocols.Combined.get (read (m !! 2)) (read (t !! 2))) (Protocols.Tuple.get (Protocols.Combined.get (read (m !! 3)) (read (t !! 3))) (Protocols.Combined.get (read (m !! 4)) (read (t !! 4))))))
                         in case getArgWithDefault args "" (longOption "sniper") of
                              "" ->
                                if args `isPresent` longOption "manual"
                                  then do
                                    result <- simulate proto (map read x) (Snipers.manualSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                                    end result (map read x) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 0)) (read (t !! 0))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 1)) (read (t !! 1))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 2)) (read (t !! 2))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 3)) (read (t !! 3))) (Protocols.Combined.test (read (m !! 4)) (read (t !! 4)))))))
                                  else do
                                    result <- simulate proto (map read x) (Snipers.noSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
                                    end result (map read x) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 0)) (read (t !! 0))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 1)) (read (t !! 1))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 2)) (read (t !! 2))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 3)) (read (t !! 3))) (Protocols.Combined.test (read (m !! 4)) (read (t !! 4)))))))
                              rt -> do
                                result <- simulate proto (map read x) (Snipers.randomSniper seed (read rt)) seed delay (not (args `isPresent` longOption "nocheck")) True
                                end result (map read x) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 0)) (read (t !! 0))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 1)) (read (t !! 1))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 2)) (read (t !! 2))) (Protocols.Tuple.test (Protocols.Combined.test (read (m !! 3)) (read (t !! 3))) (Protocols.Combined.test (read (m !! 4)) (read (t !! 4)))))))
                      _ -> putStrLn "A maximum of 5 protocols to combine is supported" >> return ()
    )

  when (args `isPresent` command "cutstat") $ do
    x0Max <- args `getArgOrExit` argument "x0Max"
    tMin <- args `getArgOrExit` argument "tMin"
    tMax <- args `getArgOrExit` argument "tMax"
    rt <- args `getArgOrExit` longOption "sniper"
    seed <- getSeed args
    path <- getFilePath args
    writeFile path "Sn Mn Rt\n"
      >> mapM
        ( \(x0, t) -> do
            (output, _, snipes) <- simulate (Protocols.Cut.get t) [x0] (Snipers.randomSniper seed (read rt)) seed 0 (not (args `isPresent` longOption "nocheck")) False
            return (snipes, Protocols.Cut.test t [x0] output)
        )
        (concatMap (\x0 -> map (x0,) [(read tMin) .. (read tMax)]) [2 .. read x0Max])
      >>= formatDat path

  when (args `isPresent` command "modstat") $ do
    x0Max <- args `getArgOrExit` argument "x0Max"
    mMin <- args `getArgOrExit` argument "mMin"
    mMax <- args `getArgOrExit` argument "mMax"
    rt <- args `getArgOrExit` longOption "sniper"
    seed <- getSeed args
    path <- getFilePath args
    writeFile path "Sn Mn Rt\n"
      >> mapM
        ( \(x0, m) -> do
            (output, _, snipes) <- simulate (Protocols.Modulo.get m) [x0] (Snipers.randomSniper seed (read rt)) seed 0 (not (args `isPresent` longOption "nocheck")) False
            return (snipes, Protocols.Modulo.test m [x0] output)
        )
        (concatMap (\x0 -> map (x0,) [(read mMin) .. (read mMax)]) [2 .. read x0Max])
      >>= formatDat path

  when (args `isPresent` command "cmbstat") $ do
    x0Max <- args `getArgOrExit` argument "x0Max"
    mMin <- args `getArgOrExit` argument "mMin"
    mMax <- args `getArgOrExit` argument "mMax"
    tMin <- args `getArgOrExit` argument "tMin"
    tMax <- args `getArgOrExit` argument "tMax"
    rt <- args `getArgOrExit` longOption "sniper"
    seed <- getSeed args
    path <- getFilePath args
    writeFile path "Sn Mn Rt\n"
      >> mapM
        ( \(x0, m, t) -> do
            (output, _, snipes) <- simulate (Protocols.Combined.get m t) [x0] (Snipers.randomSniper seed (read rt)) seed 0 (not (args `isPresent` longOption "nocheck")) False
            return (snipes, Protocols.Combined.test m t [x0] output)
        )
        (concatMap (\(x0, m) -> map (x0,m,) [(read tMin) .. (read tMax)]) (concatMap (\x0 -> map (x0,) [(read mMin) .. (read mMax)]) [2 .. read x0Max]))
      >>= formatDat path

  when (args `isPresent` command "pstat") $ do
    xMax <- args `getArgOrExit` argument "x0Max"
    mMin <- args `getArgOrExit` argument "mMin"
    mMax <- args `getArgOrExit` argument "mMax"
    tMin <- args `getArgOrExit` argument "tMin"
    tMax <- args `getArgOrExit` argument "tMax"
    rt <- args `getArgOrExit` longOption "sniper"
    seed <- getSeed args
    path <- getFilePath args
    writeFile path "Sn Mn Rt\n"
      >> mapM
        ( \((x0, x1, x2), (m0, m1, m2), (t0, t1, t2)) -> do
            (output, _, snipes) <- simulate (Protocols.Tuple.get (Protocols.Combined.get m0 t0) (Protocols.Tuple.get (Protocols.Combined.get m1 t1) (Protocols.Combined.get m2 t2))) [x0, x1, x2] (Snipers.randomSniper seed (read rt)) seed 0 (not (args `isPresent` longOption "nocheck")) False
            return (snipes, Protocols.Tuple.test (Protocols.Combined.test m0 t0) (Protocols.Tuple.test (Protocols.Combined.test m1 t1) (Protocols.Combined.test m2 t2)) [x0, x1, x2] output)
        )
        (cartProd3 (filter (\(x0, x1, x2) -> x0 <= x1 && x1 <= x2) (cartProd3self [1 .. read xMax])) (filter (\(m0, m1, m2) -> m0 < m1 && m1 < m2) (cartProd3self [read mMin .. read mMax])) (filter (\(t0, t1, t2) -> t0 < t1 && t1 < t2) (cartProd3self [read tMin .. read tMax])))
      >>= formatDat path

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

getFilePath :: Arguments -> IO String
getFilePath args = getArgOrExit args (longOption "path")

simulate :: (Eq a, Show a, Eq b, Show b, Ord b) => Protocol a b -> [Int] -> Sniper a s -> Int -> Int -> Bool -> Bool -> IO (b, Colour, Int)
simulate (Protocol m d s o t f) x sn seed delay doCheck doPrint =
  let helper (Protocol m d s o t f) states sniper snipes gen =
        if isStable doCheck states (Protocol m d s o t f)
          then
            let Just (output, colour) = getOutput states (Protocol m d s o t f)
             in return (output, colour, snipes)
          else do
            (newStates, newSniper, snipeAmount, newGen) <- step states (Protocol m d s o t f) sniper gen delay doCheck doPrint
            helper (Protocol m d s o t f) newStates newSniper (snipes + snipeAmount) newGen
   in do
        putStr "\nInput: "
        print x
        when doPrint $ do
          putStrLn ""
        helper (Protocol m d s o t f) (Protocols.getInitial (Protocol m d s o t f) x) sn 0 (mkStdGen seed)

step :: (Eq a, Show a, Eq b, Show b, Ord b) => Configuration a -> Protocol a b -> Sniper a s -> StdGen -> Int -> Bool -> Bool -> IO (Configuration a, Sniper a s, Int, StdGen)
step states (Protocol mapping delta stringify output test function) (Sniper sniperState sniping) gen delay doCheck doPrint = do
  let (a1, a2, newGen) = selectAgents states (Protocol mapping delta stringify output test function) gen
   in let (q1, q2) = Protocols.deltaWrapper delta (states !! a1) (states !! a2)
       in let newStates = replace a1 q1 (replace a2 q2 states)
           in do
                when doPrint $ do
                  printConfig states a1 a2 stringify delay
                  printConfig newStates (-1) (-1) stringify delay
                if not (isStable doCheck newStates (Protocol mapping delta stringify output test function))
                  then do
                    (newSniperState, killed) <- sniping newStates sniperState
                    case killed of
                      Just i ->
                        if (case newStates !! i of (b, _) -> b)
                          then return (replace i (False, let (_, s) = newStates !! i in s) newStates, (Sniper newSniperState sniping), 1, newGen)
                          else return (newStates, (Sniper newSniperState sniping), 0, newGen)
                      Nothing -> return (newStates, (Sniper newSniperState sniping), 0, newGen)
                  else return (newStates, (Sniper sniperState sniping), 0, newGen)

isStable :: (Eq a, Show a, Eq b, Show b, Ord b) => Bool -> Configuration a -> Protocol a b -> Bool
isStable doCheck c (Protocol m d s o t f) =
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
             in case find (any (\state -> output /= (case o state of (r, _) -> r))) successors of
                  Just _ -> False
                  Nothing -> helperCheck (queue ++ Data.List.filter (\state -> notElem state found && notElem state queue) successors) (current : found) output
          helperNoCheck [] _ =
            True
          helperNoCheck (first : states) delta =
            ( not
                ( Data.List.any
                    ( \q ->
                        ( let (q1, q2) = delta first q
                           in not ((q1 == first && q2 == q) || (q1 == q && q2 == first))
                        )
                    )
                    states
                )
                && helperNoCheck states delta
            )
       in case getOutput c (Protocol m d s o t f) of
            Just r ->
              if doCheck
                then helperCheck [Data.Maybe.mapMaybe (\(b, state) -> if b then Just state else Nothing) c] [] (case r of (res, _) -> res)
                else helperNoCheck (Data.Maybe.mapMaybe (\(b, state) -> if b then Just state else Nothing) c) d
            Nothing -> False

selectAgents :: (Eq a, Show a, Eq b, Show b) => Configuration a -> Protocol a b -> StdGen -> (Int, Int, StdGen)
selectAgents states (Protocol _ delta _ _ _ f) g =
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

getOutput :: (Eq a, Show a, Eq b, Show b, Ord b) => Configuration a -> Protocol a b -> Maybe (b, Colour)
getOutput c (Protocol _ _ _ o _ _) =
  let outputs = Data.List.map head (Data.List.group (Data.Maybe.mapMaybe (\(b, s) -> if b then Just (o s) else Nothing) c))
   in case outputs of
        [(r, colour)] -> Just (r, colour)
        xs -> case Data.List.map head (Data.List.group (Data.List.sort (map fst xs))) of
          [r] -> Just (r, white)
          _ -> Nothing

end :: (Show b) => (b, Colour, Int) -> [Int] -> ([Int] -> b -> Int) -> IO ()
end (output, colour, snipes) x t =
  putStrLn ""
    >> putChunksUtf8With With24BitColours [chunk (pack "Output: "), fore colour (chunk (pack (show output)))]
    >> putStrLn ("\nMinimum required snipes: " ++ show (t x output))
    >> putStrLn ("Actual amount of snipes: " ++ show snipes)

endWithoutTest :: (Show b) => (b, Colour, Int) -> IO ()
endWithoutTest (output, colour, snipes) =
  putStrLn ""
    >> putChunksUtf8With With24BitColours [chunk (pack "Output: "), fore colour (chunk (pack (show output)))]
    >> putStrLn ("Amount of snipes: " ++ show snipes)

formatDat :: FilePath -> [(Int, Int)] -> IO ()
formatDat path [] =
  appendFile path ""
formatDat path ((snipes, minimal) : xs) =
  if snipes == 0
    then formatDat path xs
    else appendFile path (show snipes ++ " " ++ show minimal ++ " " ++ show (fromIntegral minimal / fromIntegral snipes) ++ "\n") >> formatDat path xs
