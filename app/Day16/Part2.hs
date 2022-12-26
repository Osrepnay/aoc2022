module Day16.Part2 (thisMain) where

import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import qualified Data.List       as L
import           Data.List.Split
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S
import           System.IO

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
    let aaFlows = zerolessFlows ! "AA"
    let aaDists = M.delete "AA" $ zerolessDists ! "AA"
    let someAaFlows = M.insert "AA" aaFlows aalessFlows
    let someAaDists = M.insert "AA" aaDists aalessDists
    print $ maxPressure someAaFlows someAaDists 26 [("AA", 0), ("AA", 0)]
    hClose handle

maxPressure
    :: Map String Int
    -> Map String (Map String Int)
    -> Int
    -> [(String, Int)]
    -> Int
maxPressure flows dists mins valvesOn
    | mins <= 0 = 0
    | null unfrozen = maxPressure
        flows
        dists
        (mins - minFrozen)
        (second (subtract minFrozen) <$> frozen)
    | otherwise = maximum
        (0 :
            [ totalReleased + maxPressure
                (M.union (M.fromList (second (const 0) <$> ds)) flows)
                dists
                (mins - 1)
                ((second (subtract 1) <$> frozen) ++ ds)
            | ds <- distArrangements
            , length (L.nubBy ((==) `on` fst) ds) == length ds
            , let dsFlows = (flows !) . fst <$> ds
            , 0 `notElem` dsFlows
            , let totalReleased = sum
                    $ (\(d, f) -> (mins - d - 1) * f)
                    <$> zipWith (\(_, a) b -> (a, b)) ds dsFlows
            ])
  where
    distArrangements = traverse M.toList unfrozenDists
    unfrozenDists = (dists !) <$> unfrozenValves
    minFrozen = snd (minimum frozen)
    unfrozenValves = fst <$> unfrozen
    (unfrozen, frozen) = L.partition ((== 0) . snd) valvesOn

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
