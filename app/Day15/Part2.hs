module Day15.Part2 (thisMain) where

import qualified Data.List  as L
import           Data.Maybe
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day15" ReadMode
    contents <- hGetContents handle
    let sensors = parseSensor <$> lines contents
    let ranges =
            (\y ->
                mergeRanges $ L.sort $ catMaybes $ rangeAtY y <$> sensors)
            <$> [0..4000000]
    let nononeIdx = fromJust $ L.findIndex ((> 1) . length) ranges
    let nonone = head $ ranges !! nononeIdx
    print
        $ 4000000 * toInteger (rangeStart nonone + rangeLen nonone + 1)
        + toInteger nononeIdx
    hClose handle

data Sensor = Sensor
    { _sensorCenter :: (Int, Int)
    , _sensorDist   :: Int
    } deriving (Show)
data Range = Range
    { rangeStart :: Int
    , rangeLen   :: Int
    } deriving (Eq, Ord, Show)

mergeRanges :: [Range] -> [Range]
mergeRanges [] = []
mergeRanges [x] = [x]
mergeRanges (ar@(Range ax ad) : br@(Range bx bd) : rs)
    | aEnd >= (bx - 1) = mergeRanges $ Range minStart (maxEnd - minStart) : rs
    | otherwise        = ar : mergeRanges (br : rs)
  where
    minStart = min ax bx
    maxEnd = max aEnd (bx + bd)
    aEnd = (ax + ad)

rangeAtY :: Int -> Sensor -> Maybe Range
rangeAtY y (Sensor (sx, sy) d)
    | yDist >= d = Nothing
    | otherwise  = Just (Range (sx - diff) (diff * 2))
  where
    yDist = abs (sy - y)
    diff = d - yDist

parseSensor :: String -> Sensor
parseSensor line = Sensor startPos endDist
  where
    worded = words line
    startWords = take 2 $ drop 2 worded
    startPos =
        ( read $ init $ drop 2 $ head startWords
        , read $ init $ drop 2 $ last startWords
        )
    endWords = take 2 $ drop 8 worded
    endPos =
        (read $ init $ drop 2 $ head endWords
        , read $ drop 2 $ last endWords
        )
    endDist = (abs $ (fst startPos - fst endPos))
        + (abs $ (snd startPos - snd endPos))
