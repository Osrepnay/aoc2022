module Day1.Part2 (thisMain) where

import qualified Data.List       as L
import           Data.List.Split
import           System.IO

thisMain :: IO ()
thisMain = do
    handle <- openFile "inputs/day1" ReadMode
    contents <- hGetContents handle
    let elves = fmap read . lines <$> splitOn "\n\n" contents
    print $ biggestElvesCalories elves
    hClose handle

biggestElvesCalories :: [[Int]] -> Int
biggestElvesCalories = sum . take 3 . L.sortBy (flip compare) . fmap sum
