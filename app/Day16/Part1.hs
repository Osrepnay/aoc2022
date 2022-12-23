module Day16.Part1 (thisMain) where

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
    print $ maxPressure valveFlows valveDists 30 "AA"
    hClose handle

maxPressure
    :: Map String Int
    -> Map String (Map String Int)
    -> Int
    -> String
    -> Int
maxPressure valveFlows valveDists minutes valve
    | minutes <= 0 || (thisFlow == 0 && valve /= "AA") = 0
    | otherwise = thisFlow * minutes
        + maximum
            (M.mapWithKey
            (\k a -> maxPressure flowsOpened valveDists (minutes - a - 1) k)
            thisDists)
  where
    flowsOpened = M.insert valve 0 valveFlows
    thisFlow = valveFlows ! valve
    thisDists = valveDists ! valve

distsTo
    :: Map String (Set String)
    -> String
    -> Map String Int
distsTo valveConns start = go
    (S.fromList $ M.keys valveConns)
    (M.insert start 0 (M.map (const maxBound) valveConns))
    start
  where
    go :: Set String -> Map String Int -> String -> Map String Int
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
