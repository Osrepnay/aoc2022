module Day16.Part2 (thisMain) where

import           Data.Foldable
import           Data.Function
import           Data.List.Split
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S
import           System.IO

import Debug.Trace

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day16" ReadMode
    contents <- hGetContents handle
    let valvesAll = M.fromList $ parseValve <$> lines contents
    let valveFlows = M.map fst valvesAll
    let valveConns = M.map snd valvesAll
    let valveDists = M.fromList
            $ (\k -> (k, distsTo valveConns k))
            <$> M.keys valvesAll
    let zeroFlows = M.keys $ M.delete "AA" $ M.filter (== 0) valveFlows
    let (zerolessFlows, zerolessDists) = foldl'
            (flip wipe)
            (valveFlows, valveDists)
            zeroFlows
    let (aalessFlows, aalessDists) = wipe "AA" (zerolessFlows, zerolessDists)
    print zerolessFlows
    print zerolessDists
    let aaConns = M.toList $ M.delete "AA" $ zerolessDists ! "AA"
    print
        $ maximum
        $ (\(v, d) -> maxPressure aalessFlows aalessDists (60 - d - 1) v)
        <$> aaConns
    hClose handle

maxPressure
    :: Map String Int
    -> Map String (Map String Int)
    -> Int
    -> String
    -> Int
maxPressure valveFlows valveDists minutes valve
    | minutes <= 0 = 0
    | thisFlow == 0 = 0
    | otherwise = thisFlow * minutes
        + maximum
            (M.mapWithKey
            (\k a -> maxPressure flowsOpened valveDists (minutes - a - 1) k)
            thisDists)
  where
    flowsOpened = M.insert valve 0 valveFlows
    thisFlow = valveFlows ! valve
    thisDists = valveDists ! valve

wipe
    :: String
    -> (Map String Int, Map String (Map String Int))
    -> (Map String Int, Map String (Map String Int))
wipe valve (valveFlows, valveDists) =
    ( filterKeyValve valveFlows
    , M.map filterKeyValve $ filterKeyValve valveDists
    )
  where
    filterKeyValve = M.filterWithKey (\k _ -> k /= valve)

distsTo
    :: Map String (Set String)
    -> String
    -> Map String Int
distsTo valveConns start = go
    (S.fromList $ M.keys valveConns)
    (M.insert start 0 (M.map (const maxBound) valveConns))
    start
  where
    go unvisited dists valve
        | S.null unvisited = dists
        | otherwise        = go
            (S.delete valve unvisited)
            mergedDists
            (minimumBy (compare `on` (mergedDists !)) unvisited)
      where
        mergedDists = M.unionWith min dists thisConnDists
        thisConnDists = M.fromList $ (\x -> (x, thisDist + 1)) <$> S.toList thisConns
        thisDist = dists ! valve
        thisConns = S.intersection unvisited (valveConns ! valve)

parseValve :: String -> (String, (Int, Set String))
parseValve s =
    ( worded !! 1
    , ( read $ drop 5 $ init $ worded !! 4
      , S.fromList $ splitOn ", " end
      )
    )
  where
    worded = words s
    end = unwords $ drop 9 worded
