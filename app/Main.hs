{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Control.Monad
import Data.List
import Data.List.Duplicate
import Data.Maybe
import Data.Text hiding (any, concat, concatMap, find, head, length, map, null, replace, replicate, zipWith, show)
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
import Data.Dynamic

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
          "" -> do
            result <- simulate proto [read x0] (if args `isPresent` longOption "manual" then Snipers.manualSniper else Snipers.noSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
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
          "" -> do
            result <- simulate proto [read x0] (if args `isPresent` longOption "manual" then Snipers.manualSniper else Snipers.noSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
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
          "" -> do
            result <- simulate proto [read x0] (if args `isPresent` longOption "manual" then Snipers.manualSniper else Snipers.noSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
            end result [read x0] (Protocols.Combined.test (read m) (read t))
          rt -> do
            result <- simulate proto [read x0] (Snipers.randomSniper seed (read rt)) seed delay (not (args `isPresent` longOption "nocheck")) True
            end result [read x0] (Protocols.Combined.test (read m) (read t))

  when (args `isPresent` command "mdc") $ do
    seed <- getSeed args
    delay <- getDelay args
    let x = args `getAllArgs` argument "xi"
     in let m = args `getAllArgs` argument "mi"
      in let t = args `getAllArgs` argument "ti"
       in let p1 : p2 : unary = zipWith (\ mi ti -> Protocols.Combined.get (read mi) (read ti)) m t
        in let Just proto = fromDynamic (Data.List.foldl (\acc a -> toDyn (Protocols.Tuple.get (let Just p = (fromDynamic acc) in p) a)) (toDyn (Protocols.Tuple.get p1 p2)) unary)
         in case getArgWithDefault args "" (longOption "sniper") of
          "" -> do
            result <- simulate proto (map read x) (if args `isPresent` longOption "manual" then Snipers.manualSniper else Snipers.noSniper) seed delay (not (args `isPresent` longOption "nocheck")) True
            endWithoutTest result
          rt -> do
            result <- simulate proto (map read x) (Snipers.randomSniper seed (read rt)) seed delay (not (args `isPresent` longOption "nocheck")) True
            endWithoutTest result

  when (args `isPresent` command "cut-stat") $ do
    x0Max <- args `getArgOrExit` argument "x0Max"
    tMin <- args `getArgOrExit` argument "tMin"
    tMax <- args `getArgOrExit` argument "tMax"
    rt <- args `getArgOrExit` longOption "sniper"
    seed <- getSeed args
    path <- getFilePath args
    writeFile path "Sn Mn Rt\n" >> mapM
      ( \(x0, t) -> do
          (output, _, snipes) <- simulate (Protocols.Cut.get t) [x0] (Snipers.randomSniper seed (read rt)) seed 0 (not (args `isPresent` longOption "nocheck")) False
          return (snipes, Protocols.Cut.test t [x0] output)
      )
      (concatMap (\x0 -> map (x0,) [(read tMin) .. (read tMax)]) [2 .. read x0Max])
      >>= formatDat path

  when (args `isPresent` command "mod-stat") $ do
    x0Max <- args `getArgOrExit` argument "x0Max"
    mMin <- args `getArgOrExit` argument "mMin"
    mMax <- args `getArgOrExit` argument "mMax"
    rt <- args `getArgOrExit` longOption "sniper"
    seed <- getSeed args
    path <- getFilePath args
    writeFile path "Sn Mn Rt\n" >> mapM
      ( \(x0, m) -> do
          (output, _, snipes) <- simulate (Protocols.Modulo.get m) [x0] (Snipers.randomSniper seed (read rt)) seed 0 (not (args `isPresent` longOption "nocheck")) False
          return (snipes, Protocols.Modulo.test m [x0] output)
      )
      (concatMap (\x0 -> map (x0,) [(read mMin) .. (read mMax)]) [2 .. read x0Max])
      >>= formatDat path

  when (args `isPresent` command "cmb-stat") $ do
    x0Max <- args `getArgOrExit` argument "x0Max"
    mMin <- args `getArgOrExit` argument "mMin"
    mMax <- args `getArgOrExit` argument "mMax"
    tMin <- args `getArgOrExit` argument "tMin"
    tMax <- args `getArgOrExit` argument "tMax"
    rt <- args `getArgOrExit` longOption "sniper"
    seed <- getSeed args
    path <- getFilePath args
    writeFile path "Sn Mn Rt\n" >> mapM
      ( \(x0, m, t) -> do
          (output, _, snipes) <- simulate (Protocols.Combined.get m t) [x0] (Snipers.randomSniper seed (read rt)) seed 0 (not (args `isPresent` longOption "nocheck")) False
          return (snipes, Protocols.Combined.test m t [x0] output)
      )
      (concatMap (\(x0, m) -> map (x0,m,) [(read tMin) .. (read tMax)]) (concatMap (\x0 -> map (x0,) [(read mMin) .. (read mMax)]) [2 .. read x0Max]))
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
simulate (Protocol m d s o t n) x sn seed delay doCheck doPrint =
  let helper (Protocol m d s o t n) states sniper snipes gen =
        if isStable doCheck states (Protocol m d s o t n)
          then
            let Just (output, colour) = getOutput states (Protocol m d s o t n)
             in return (output, colour, snipes)
          else do
            (newStates, newSniper, snipeAmount, newGen) <- step states (Protocol m d s o t n) sniper gen delay doCheck doPrint
            helper (Protocol m d s o t n) newStates newSniper (snipes + snipeAmount) newGen
   in do
        when doPrint $ do
          putStrLn ""
        helper (Protocol m d s o t n) (Protocols.getInitial (Protocol m d s o t n) x) sn 0 (mkStdGen seed)

step :: (Eq a, Show a, Eq b, Show b, Ord b) => Configuration a -> Protocol a b -> Sniper a s -> StdGen -> Int -> Bool -> Bool -> IO (Configuration a, Sniper a s, Int, StdGen)
step states (Protocol mapping delta stringify output test n) (Sniper sniperState sniping) gen delay doCheck doPrint = do
  let (a1, a2, newGen) = selectAgents states (Protocol mapping delta stringify output test n) gen
   in let (q1, q2) = Protocols.deltaWrapper delta (states !! a1) (states !! a2)
       in let newStates = replace a1 q1 (replace a2 q2 states)
           in do
                when doPrint $ do
                  printConfig states a1 a2 stringify delay
                  printConfig newStates (-1) (-1) stringify delay
                if not (isStable doCheck newStates (Protocol mapping delta stringify output test n))
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
isStable doCheck c (Protocol m d s o t n) =
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
       in case getOutput c (Protocol m d s o t n) of
            Just r ->
              if doCheck
                then helperCheck [Data.Maybe.mapMaybe (\(b, state) -> if b then Just state else Nothing) c] [] (case r of (res, _) -> res)
                else helperNoCheck (Data.Maybe.mapMaybe (\(b, state) -> if b then Just state else Nothing) c) d
            Nothing -> False

selectAgents :: (Eq a, Show a, Eq b, Show b) => Configuration a -> Protocol a b -> StdGen -> (Int, Int, StdGen)
selectAgents states (Protocol _ delta _ _ _ _) g =
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
          _   -> Nothing

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
formatDat path ((snipes, mininal) : xs) =
  if snipes == 0
    then formatDat path xs
    else appendFile path (show snipes ++ " " ++ show mininal ++ " " ++ show (fromIntegral mininal / fromIntegral snipes) ++ "\n") >> formatDat path xs